-module(hikyaku_email).

-include("hikyaku.hrl").

-define(HEADER_NAME_RE, ~"\\A[!-9;-~]{1,76}\\z").
%% Any control character except horizontal tab.
-define(HEADER_VALUE_FORBIDDEN_RE, ~"[\\x00-\\x08\\x0a-\\x1f\\x7f]").
%% Derived from other fields, or set by SendGrid itself.
-define(RESERVED_HEADERS, [
    ~"from",
    ~"to",
    ~"cc",
    ~"bcc",
    ~"reply-to",
    ~"subject",
    ~"content-type",
    ~"mime-version",
    ~"content-transfer-encoding",
    ~"x-sg-id",
    ~"x-sg-eid",
    ~"received",
    ~"dkim-signature"
]).

-export([
    new/0,
    from/2,
    to/2,
    cc/2,
    bcc/2,
    reply_to/2,
    subject/2,
    text_body/2,
    html_body/2,
    attachment/2,
    header/3,
    provider_option/3,
    put_to/2,
    put_cc/2,
    put_bcc/2
]).

-export([
    subject/1,
    from/1,
    to/1,
    cc/1,
    bcc/1,
    reply_to/1,
    text_body/1,
    html_body/1,
    attachments/1,
    headers/1,
    provider_options/1
]).

-type address_input() ::
    binary()
    | {binary(), binary()}
    | #hikyaku_address{}.

-export_type([address_input/0]).

-spec new() -> #hikyaku_email{}.
new() ->
    #hikyaku_email{}.

-doc "Sets the sender. Raises `{invalid_address, address | name, Value}` for an address or display name that is not a single valid mailbox.".
-spec from(#hikyaku_email{}, address_input()) -> #hikyaku_email{}.
from(Email, Input) ->
    Email#hikyaku_email{from = normalize_address(Input)}.

-doc "Appends a `To` recipient. Raises `{invalid_address, address | name, Value}` for an address or display name that is not a single valid mailbox.".
-spec to(#hikyaku_email{}, address_input()) -> #hikyaku_email{}.
to(Email = #hikyaku_email{to = Current}, Input) ->
    Email#hikyaku_email{to = Current ++ [normalize_address(Input)]}.

-doc "Appends a `Cc` recipient. Raises `{invalid_address, address | name, Value}` for an address or display name that is not a single valid mailbox.".
-spec cc(#hikyaku_email{}, address_input()) -> #hikyaku_email{}.
cc(Email = #hikyaku_email{cc = Current}, Input) ->
    Email#hikyaku_email{cc = Current ++ [normalize_address(Input)]}.

-doc "Appends a `Bcc` recipient. Raises `{invalid_address, address | name, Value}` for an address or display name that is not a single valid mailbox.".
-spec bcc(#hikyaku_email{}, address_input()) -> #hikyaku_email{}.
bcc(Email = #hikyaku_email{bcc = Current}, Input) ->
    Email#hikyaku_email{bcc = Current ++ [normalize_address(Input)]}.

-doc "Sets the `Reply-To` address. Raises `{invalid_address, address | name, Value}` for an address or display name that is not a single valid mailbox.".
-spec reply_to(#hikyaku_email{}, address_input()) -> #hikyaku_email{}.
reply_to(Email, Input) ->
    Email#hikyaku_email{reply_to = normalize_address(Input)}.

-spec subject(#hikyaku_email{}, binary()) -> #hikyaku_email{}.
subject(Email, Subject) when is_binary(Subject) ->
    Email#hikyaku_email{subject = Subject}.

-spec text_body(#hikyaku_email{}, binary()) -> #hikyaku_email{}.
text_body(Email, Body) when is_binary(Body) ->
    Email#hikyaku_email{text_body = Body}.

-spec html_body(#hikyaku_email{}, binary()) -> #hikyaku_email{}.
html_body(Email, Body) when is_binary(Body) ->
    Email#hikyaku_email{html_body = Body}.

-doc """
Adds an attachment.

Raises `{invalid_attachment, Field, Value}` if the attachment fails the
checks in `hikyaku_attachment:validate/1`.
""".
-spec attachment(#hikyaku_email{}, #hikyaku_attachment{}) -> #hikyaku_email{}.
attachment(Email = #hikyaku_email{attachments = Current}, Att = #hikyaku_attachment{}) ->
    Email#hikyaku_email{attachments = Current ++ [hikyaku_attachment:validate(Att)]}.

-doc """
Sets a custom header.

The name must be 1 to 76 printable ASCII characters without `:`, and the
value must not contain control characters other than tab; otherwise
`{invalid_header, Name}` is raised. Headers that hikyaku sets from other
fields (`From`, `To`, `Cc`, `Bcc`, `Reply-To`, `Subject`, `Content-Type`,
`MIME-Version`, `Content-Transfer-Encoding`) or that providers set
themselves (`Received`, `DKIM-Signature`, SendGrid's `X-SG-ID` and
`X-SG-EID`) raise `{reserved_header, Name}`.
""".
-spec header(#hikyaku_email{}, binary(), binary()) -> #hikyaku_email{}.
header(Email = #hikyaku_email{headers = Headers}, Key, Value) when
    is_binary(Key), is_binary(Value)
->
    case
        re:run(Key, ?HEADER_NAME_RE) =/= nomatch andalso
            re:run(Value, ?HEADER_VALUE_FORBIDDEN_RE) =:= nomatch
    of
        false -> error({invalid_header, Key});
        true -> ok
    end,
    case lists:member(string:lowercase(Key), ?RESERVED_HEADERS) of
        true -> error({reserved_header, Key});
        false -> Email#hikyaku_email{headers = Headers#{Key => Value}}
    end.

-spec provider_option(#hikyaku_email{}, atom(), term()) -> #hikyaku_email{}.
provider_option(Email = #hikyaku_email{provider_options = Opts}, Key, Value) when is_atom(Key) ->
    Email#hikyaku_email{provider_options = Opts#{Key => Value}}.

-doc "Replaces the `To` recipients. Raises `{invalid_address, address | name, Value}` for an address or display name that is not a single valid mailbox.".
-spec put_to(#hikyaku_email{}, [address_input()]) -> #hikyaku_email{}.
put_to(Email, Addresses) when is_list(Addresses) ->
    Email#hikyaku_email{to = [normalize_address(A) || A <- Addresses]}.

-doc "Replaces the `Cc` recipients. Raises `{invalid_address, address | name, Value}` for an address or display name that is not a single valid mailbox.".
-spec put_cc(#hikyaku_email{}, [address_input()]) -> #hikyaku_email{}.
put_cc(Email, Addresses) when is_list(Addresses) ->
    Email#hikyaku_email{cc = [normalize_address(A) || A <- Addresses]}.

-doc "Replaces the `Bcc` recipients. Raises `{invalid_address, address | name, Value}` for an address or display name that is not a single valid mailbox.".
-spec put_bcc(#hikyaku_email{}, [address_input()]) -> #hikyaku_email{}.
put_bcc(Email, Addresses) when is_list(Addresses) ->
    Email#hikyaku_email{bcc = [normalize_address(A) || A <- Addresses]}.

-doc "Returns the subject.".
-spec subject(#hikyaku_email{}) -> binary().
subject(#hikyaku_email{subject = Subject}) ->
    Subject.

-doc "Returns the sender, or `undefined` if none is set.".
-spec from(#hikyaku_email{}) -> #hikyaku_address{} | undefined.
from(#hikyaku_email{from = From}) ->
    From.

-doc "Returns the `To` recipients in the order they were added.".
-spec to(#hikyaku_email{}) -> [#hikyaku_address{}].
to(#hikyaku_email{to = To}) ->
    To.

-doc "Returns the `Cc` recipients in the order they were added.".
-spec cc(#hikyaku_email{}) -> [#hikyaku_address{}].
cc(#hikyaku_email{cc = Cc}) ->
    Cc.

-doc "Returns the `Bcc` recipients in the order they were added.".
-spec bcc(#hikyaku_email{}) -> [#hikyaku_address{}].
bcc(#hikyaku_email{bcc = Bcc}) ->
    Bcc.

-doc "Returns the `Reply-To` address, or `undefined` if none is set.".
-spec reply_to(#hikyaku_email{}) -> #hikyaku_address{} | undefined.
reply_to(#hikyaku_email{reply_to = ReplyTo}) ->
    ReplyTo.

-doc "Returns the plain-text body, or `undefined` if none is set.".
-spec text_body(#hikyaku_email{}) -> binary() | undefined.
text_body(#hikyaku_email{text_body = Body}) ->
    Body.

-doc "Returns the HTML body, or `undefined` if none is set.".
-spec html_body(#hikyaku_email{}) -> binary() | undefined.
html_body(#hikyaku_email{html_body = Body}) ->
    Body.

-doc "Returns the attachments in the order they were added.".
-spec attachments(#hikyaku_email{}) -> [#hikyaku_attachment{}].
attachments(#hikyaku_email{attachments = Attachments}) ->
    Attachments.

-doc "Returns the custom headers.".
-spec headers(#hikyaku_email{}) -> #{binary() => binary()}.
headers(#hikyaku_email{headers = Headers}) ->
    Headers.

-doc "Returns the provider-specific options.".
-spec provider_options(#hikyaku_email{}) -> map().
provider_options(#hikyaku_email{provider_options = Opts}) ->
    Opts.

-spec normalize_address(address_input()) -> #hikyaku_address{}.
normalize_address(#hikyaku_address{name = Name, address = Address}) ->
    hikyaku_address:new(Name, Address);
normalize_address({Name, Address}) when is_binary(Name), is_binary(Address) ->
    hikyaku_address:new(Name, Address);
normalize_address(Address) when is_binary(Address) ->
    hikyaku_address:new(undefined, Address).
