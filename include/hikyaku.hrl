-ifndef(HIKYAKU_HRL).
-define(HIKYAKU_HRL, true).

-record(hikyaku_address, {
    name :: binary() | undefined,
    address :: binary()
}).

-record(hikyaku_attachment, {
    filename :: binary(),
    content_type :: binary(),
    data :: binary() | undefined,
    path :: binary() | undefined,
    disposition = attachment :: attachment | inline,
    cid :: binary() | undefined
}).

-record(hikyaku_email, {
    subject = <<>> :: binary(),
    from :: #hikyaku_address{} | undefined,
    to = [] :: [#hikyaku_address{}],
    cc = [] :: [#hikyaku_address{}],
    bcc = [] :: [#hikyaku_address{}],
    reply_to :: #hikyaku_address{} | undefined,
    text_body :: binary() | undefined,
    html_body :: binary() | undefined,
    attachments = [] :: [#hikyaku_attachment{}],
    headers = #{} :: #{binary() => binary()},
    provider_options = #{} :: map()
}).

-endif.
