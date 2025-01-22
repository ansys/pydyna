import typing

import keyword_generation.handlers.handler_base

def do_negative_shared_fields(kwd_data: typing.Dict):
    negative_shared_fields = kwd_data.get("negative_shared_fields", [])
    num_cards = len(kwd_data["cards"])
    options = kwd_data.get("options", [])
    option_cards = []
    for options in kwd_data.get("options", []):
        option_cards.extend(options["cards"])
    for setting in negative_shared_fields:
        indices = [-i for i in setting["cards"]]
        fields = []
        for index in indices:
            if index >= num_cards:
                for options in kwd_data.get("options", []):
                    for card in options["cards"]:
                        if card["index"] == index:
                            for field in card["fields"]:
                                if field["name"] == setting["name"]:
                                    fields.append(field)
            else:
                assert False, "TODO - support negative indices for shared fields for non-options"
        assert len(fields) > 1
        if not setting["applied_card_indices"]:
            fields[0]["card_indices"] = indices
        for field in fields[1:]:
            field["redundant"] = True


def handle_shared_field(kwd_data, settings):
    # positive card indices are applied in handler
    # negative card indices are marked and handled after transformations (after_handle)
    for setting in settings:
        setting["applied_card_indices"] = False
        cards = setting["cards"]
        num_positive = len([c for c in cards if c > 0])

        # either or - we cannot support some positive some negative in the same setting now
        assert num_positive == 0 or num_positive == len(cards)
        if num_positive > 0:
            fields = []
            for card in kwd_data["cards"]:
                for field in card["fields"]:
                    if field["name"] == setting["name"]:
                        fields.append(field)
            assert len(fields) > 1
            fields[0]["card_indices"] = cards
            setting["applied_card_indices"] = True
            for field in fields[1:]:
                field["redundant"] = True
        else:
            if "negative_shared_fields" not in kwd_data:
                kwd_data["negative_shared_fields"] = []
            kwd_data["negative_shared_fields"].append(setting)


class SharedFieldHandler(keyword_generation.handlers.handler_base.KeywordHandler):

    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
        return handle_shared_field(kwd_data, settings)

    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        return do_negative_shared_fields(kwd_data)