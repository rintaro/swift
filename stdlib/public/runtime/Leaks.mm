//===--- Leaks.mm -----------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// See Leaks.h for a description of this leaks detector.
//
//===----------------------------------------------------------------------===//

#if SWIFT_RUNTIME_ENABLE_LEAK_CHECKER

#include "Leaks.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#if SWIFT_OBJC_INTEROP
#import <objc/objc.h>
#import <objc/runtime.h>
#import <Foundation/Foundation.h>
#endif
#include <set>
#include <cstdio>
extern "C" {
#include <pthread.h>
}

using namespace swift;

//===----------------------------------------------------------------------===//
//                              Extra Interfaces
//===----------------------------------------------------------------------===//

#if SWIFT_OBJC_INTEROP
void swift_leaks_stopTrackingObjCObject(id obj);
void swift_leaks_startTrackingObjCObject(id obj);
#endif

//===----------------------------------------------------------------------===//
//                                   State
//===----------------------------------------------------------------------===//

/// A set of allocated swift only objects that we are tracking for leaks.
static Lazy<std::set<HeapObject *>> TrackedSwiftObjects;

#if SWIFT_OBJC_INTEROP
/// A set of allocated objc objects that we are tracking for leaks.
static Lazy<std::set<id>> TrackedObjCObjects;
#endif

/// Whether or not we should be collecting objects.
static bool ShouldTrackObjects = false;

/// A course grain lock that we use to synchronize our leak dictionary.
static pthread_mutex_t LeaksMutex = PTHREAD_MUTEX_INITIALIZER;

#if SWIFT_OBJC_INTEROP
/// Where we store the dealloc, alloc, and allocWithZone functions we swizzled.
static IMP old_dealloc_fun;
static IMP old_alloc_fun;
static IMP old_allocWithZone_fun;
#endif

//===----------------------------------------------------------------------===//
//                            Init and Deinit Code
//===----------------------------------------------------------------------===//

#if SWIFT_OBJC_INTEROP
static void __swift_leaks_dealloc(id self, SEL _cmd) {
  swift_leaks_stopTrackingObjCObject(self);
  ((void (*)(id, SEL))old_dealloc_fun)(self, _cmd);
}

static id __swift_leaks_alloc(id self, SEL _cmd) {
  id result = ((id (*)(id, SEL))old_alloc_fun)(self, _cmd);
  swift_leaks_startTrackingObjCObject(result);
  return result;
}

static id __swift_leaks_allocWithZone(id self, SEL _cmd, id zone) {
  id result = ((id (*)(id, SEL, id))old_allocWithZone_fun)(self, _cmd, zone);
  swift_leaks_startTrackingObjCObject(result);
  return result;
}
#endif

extern "C" void swift_leaks_startTrackingObjects(const char *name) {
  pthread_mutex_lock(&LeaksMutex);

  // First clear our tracked objects set.
  TrackedSwiftObjects->clear();
#if SWIFT_OBJC_INTEROP
  TrackedObjCObjects->clear();
#endif

  // Set that we should track objects.
  ShouldTrackObjects = true;

#if SWIFT_OBJC_INTEROP
  // Swizzle out -(void)dealloc, +(id)alloc, and +(id)allocWithZone: for our
  // custom implementations.
  IMP new_dealloc_fun = (IMP)__swift_leaks_dealloc;
  IMP new_alloc_fun = (IMP)__swift_leaks_alloc;
  IMP new_allocWithZone_fun = (IMP)__swift_leaks_allocWithZone;

  Method deallocMethod =
      class_getInstanceMethod([NSObject class], @selector(dealloc));
  Method allocMethod = class_getClassMethod([NSObject class], @selector(alloc));
  Method allocWithZoneMethod =
      class_getClassMethod([NSObject class], @selector(allocWithZone:));

  old_dealloc_fun = method_setImplementation(deallocMethod, new_dealloc_fun);
  old_alloc_fun = method_setImplementation(allocMethod, new_alloc_fun);
  old_allocWithZone_fun =
      method_setImplementation(allocWithZoneMethod, new_allocWithZone_fun);
#endif

  pthread_mutex_unlock(&LeaksMutex);
}

/// This assumes that the LeaksMutex is already being held.
static void dumpSwiftHeapObjects() {
  const char *comma = "";
  for (HeapObject *Obj : *TrackedSwiftObjects) {
    const HeapMetadata *Metadata = Obj->metadata;

    fprintf(stderr, "%s", comma);
    comma = ",";

    if (!Metadata) {
      fprintf(stderr, "{\"type\": \"null\"}");
      continue;
    }

    const char *kindDescriptor = "";
    switch (Metadata->getKind()) {
#define METADATAKIND(name, value)                                              \
  case MetadataKind::name:                                                     \
    kindDescriptor = #name;                                                    \
    break;
#include "swift/ABI/MetadataKind.def"
    }

    if (const NominalTypeDescriptor *NTD =
            Metadata->getNominalTypeDescriptor()) {
      fprintf(stderr, "{"
                      "\"type\": \"nominal\", "
                      "\"name\": \"%s\", "
                      "\"kind\": \"%s\""
                      "}",
              NTD->Name.get(), kindDescriptor);
      continue;
    }

    fprintf(stderr, "{\"type\": \"unknown\", \"kind\": \"%s\"}",
            kindDescriptor);
  }
}

#if SWIFT_OBJC_INTEROP
/// This assumes that the LeaksMutex is already being held.
static void dumpObjCHeapObjects() {
  const char *comma = "";
  for (id Obj : *TrackedObjCObjects) {
    // Just print out the class of Obj.
    fprintf(stderr, "%s\"%s\"", comma, object_getClassName(Obj));
    comma = ",";
  }
}
#endif

extern "C" int swift_leaks_stopTrackingObjects(const char *name) {
  pthread_mutex_lock(&LeaksMutex);
  unsigned Result = TrackedSwiftObjects->size();

  fprintf(stderr, "{\"name\":\"%s\",", name);
  fprintf(stderr, "\"swift_count\": %u, \"swift_objects\": [", 
          unsigned(TrackedSwiftObjects->size()));
  dumpSwiftHeapObjects();

#if SWIFT_OBJC_INTEROP
  Result += TrackedObjCObjects->size();
  fprintf(stderr, "], \"objc_count\": %u, \"objc_objects\": [", 
          unsigned(TrackedObjCObjects->size()));
  dumpObjCHeapObjects();
#endif
  fprintf(stderr, "]}"); 

  fflush(stderr);

  ShouldTrackObjects = false;
  TrackedSwiftObjects->clear();

#if SWIFT_OBJC_INTEROP
  TrackedObjCObjects->clear();

  // Undo our swizzling.
  Method deallocMethod =
      class_getInstanceMethod([NSObject class], @selector(dealloc));
  Method allocMethod = class_getClassMethod([NSObject class], @selector(alloc));
  Method allocWithZoneMethod =
      class_getClassMethod([NSObject class], @selector(allocWithZone:));

  method_setImplementation(deallocMethod, old_dealloc_fun);
  method_setImplementation(allocMethod, old_alloc_fun);
  method_setImplementation(allocWithZoneMethod, old_allocWithZone_fun);
#endif

  pthread_mutex_unlock(&LeaksMutex);
  return Result;
}

//===----------------------------------------------------------------------===//
//                               Tracking Code
//===----------------------------------------------------------------------===//

extern "C" void swift_leaks_startTrackingObject(HeapObject *Object) {
  pthread_mutex_lock(&LeaksMutex);
  if (ShouldTrackObjects) {
    TrackedSwiftObjects->insert(Object);
  }
  pthread_mutex_unlock(&LeaksMutex);
}

extern "C" void swift_leaks_stopTrackingObject(HeapObject *Object) {
  pthread_mutex_lock(&LeaksMutex);
  TrackedSwiftObjects->erase(Object);
  pthread_mutex_unlock(&LeaksMutex);
}

#if SWIFT_OBJC_INTEROP
void swift_leaks_startTrackingObjCObject(id Object) {
  pthread_mutex_lock(&LeaksMutex);
  if (ShouldTrackObjects) {
    TrackedObjCObjects->insert(Object);
  }
  pthread_mutex_unlock(&LeaksMutex);
}

void swift_leaks_stopTrackingObjCObject(id Object) {
  pthread_mutex_lock(&LeaksMutex);
  TrackedObjCObjects->erase(Object);
  pthread_mutex_unlock(&LeaksMutex);
}
#endif

#else
static char DummyDecl = '';
#endif
