
extern "C" void swift_leaks_startTrackingObjects(const char *)
    __attribute__((noinline, used));
extern "C" int swift_leaks_stopTrackingObjects(const char *)
    __attribute__((noinline, used));

extern "C" void
swift_stdlib_leaks_startTrackingObjects(const char *name) {
  swift_leaks_startTrackingObjects(name);
}

extern "C" int
swift_stdlib_leaks_stopTrackingObjects(const char *name) {
  return swift_leaks_stopTrackingObjects(name);
}
