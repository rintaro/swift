# flake8: noqa I201
from Token import SYNTAX_TOKEN_MAP
from kinds import kind_to_type, lowercase_first_word


class Child(object):
    """
    A child of a node, that may be declared optional or a token with a
    restricted subset of acceptable kinds or texts.
    """
    def __init__(self, name, kind, is_optional=False,
                 token_choices=None, text_choices=None):
        self.name = name
        self.syntax_kind = kind
        self.is_optional = is_optional

        # If the child has "token" anywhere in the kind, it's considered
        # a token node. Grab the existing reference to that token from the
        # global list.
        is_token = self.syntax_kind.endswith('Token')
        self.token_choices = []
        if is_token and self.syntax_kind != 'Token':
            token = SYNTAX_TOKEN_MAP[self.syntax_kind]
            if token:
                self.token_choices.append(token)
        for choice in token_choices or []:
            token = SYNTAX_TOKEN_MAP[choice]
            self.token_choices.append(token)
        if is_token:
            assert self.token_choices, "Token child must specify token kinds."

        # A list of valid text for tokens, if specified.
        # This will force validation logic to check the text passed into the
        # token against the choices.
        self.text_choices = text_choices or []

    def is_token(self):
        """
        Returns true if this child has a token kind.
        """
        return bool(self.token_choices)

    def main_token(self):
        """
        Returns the first choice from the token_choices if there are any,
        otherwise returns None.
        """
        if self.token_choices:
            return self.token_choices[0]
        return None
