-module(hikyaku_email).

-include("hikyaku.hrl").

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

-type address_input() ::
    binary()
    | {binary(), binary()}
    | #hikyaku_address{}.

-export_type([address_input/0]).

-spec new() -> #hikyaku_email{}.
new() ->
    #hikyaku_email{}.

-spec from(#hikyaku_email{}, address_input()) -> #hikyaku_email{}.
from(Email, Input) ->
    Email#hikyaku_email{from = normalize_address(Input)}.

-spec to(#hikyaku_email{}, address_input()) -> #hikyaku_email{}.
to(Email = #hikyaku_email{to = Current}, Input) ->
    Email#hikyaku_email{to = Current ++ [normalize_address(Input)]}.

-spec cc(#hikyaku_email{}, address_input()) -> #hikyaku_email{}.
cc(Email = #hikyaku_email{cc = Current}, Input) ->
    Email#hikyaku_email{cc = Current ++ [normalize_address(Input)]}.

-spec bcc(#hikyaku_email{}, address_input()) -> #hikyaku_email{}.
bcc(Email = #hikyaku_email{bcc = Current}, Input) ->
    Email#hikyaku_email{bcc = Current ++ [normalize_address(Input)]}.

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

-spec attachment(#hikyaku_email{}, #hikyaku_attachment{}) -> #hikyaku_email{}.
attachment(Email = #hikyaku_email{attachments = Current}, Att = #hikyaku_attachment{}) ->
    Email#hikyaku_email{attachments = Current ++ [Att]}.

-spec header(#hikyaku_email{}, binary(), binary()) -> #hikyaku_email{}.
header(Email = #hikyaku_email{headers = Headers}, Key, Value) when
    is_binary(Key), is_binary(Value)
->
    Email#hikyaku_email{headers = Headers#{Key => Value}}.

-spec provider_option(#hikyaku_email{}, atom(), term()) -> #hikyaku_email{}.
provider_option(Email = #hikyaku_email{provider_options = Opts}, Key, Value) when is_atom(Key) ->
    Email#hikyaku_email{provider_options = Opts#{Key => Value}}.

-spec put_to(#hikyaku_email{}, [address_input()]) -> #hikyaku_email{}.
put_to(Email, Addresses) when is_list(Addresses) ->
    Email#hikyaku_email{to = [normalize_address(A) || A <- Addresses]}.

-spec put_cc(#hikyaku_email{}, [address_input()]) -> #hikyaku_email{}.
put_cc(Email, Addresses) when is_list(Addresses) ->
    Email#hikyaku_email{cc = [normalize_address(A) || A <- Addresses]}.

-spec put_bcc(#hikyaku_email{}, [address_input()]) -> #hikyaku_email{}.
put_bcc(Email, Addresses) when is_list(Addresses) ->
    Email#hikyaku_email{bcc = [normalize_address(A) || A <- Addresses]}.

-spec normalize_address(address_input()) -> #hikyaku_address{}.
normalize_address(#hikyaku_address{} = Addr) ->
    Addr;
normalize_address({Name, Address}) when is_binary(Name), is_binary(Address) ->
    #hikyaku_address{name = Name, address = Address};
normalize_address(Address) when is_binary(Address) ->
    #hikyaku_address{address = Address}.
