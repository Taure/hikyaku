-module(hikyaku_attachment).

-include("hikyaku.hrl").

-define(CONTENT_TYPE_RE,
    ~"\\A[!#$%&'*+.^_`|~0-9A-Za-z-]+/[!#$%&'*+.^_`|~0-9A-Za-z-]+\\z"
).

-export([
    from_path/1,
    from_path/2,
    from_data/2,
    from_data/3,
    inline/2,
    validate/1
]).

-spec from_path(binary()) -> #hikyaku_attachment{}.
from_path(Path) when is_binary(Path) ->
    Filename = filename:basename(Path),
    ContentType = detect_content_type(Filename),
    validate(#hikyaku_attachment{
        filename = iolist_to_binary(Filename),
        content_type = ContentType,
        path = Path
    }).

-spec from_path(binary(), map()) -> #hikyaku_attachment{}.
from_path(Path, Opts) when is_binary(Path), is_map(Opts) ->
    Att = from_path(Path),
    validate(apply_opts(Att, Opts)).

-spec from_data(binary(), binary()) -> #hikyaku_attachment{}.
from_data(Filename, Data) when is_binary(Filename), is_binary(Data) ->
    ContentType = detect_content_type(Filename),
    validate(#hikyaku_attachment{
        filename = Filename,
        content_type = ContentType,
        data = Data
    }).

-spec from_data(binary(), binary(), map()) -> #hikyaku_attachment{}.
from_data(Filename, Data, Opts) when is_binary(Filename), is_binary(Data), is_map(Opts) ->
    Att = from_data(Filename, Data),
    validate(apply_opts(Att, Opts)).

-spec inline(binary(), binary()) -> #hikyaku_attachment{}.
inline(Cid, Path) when is_binary(Cid), is_binary(Path) ->
    Filename = filename:basename(Path),
    ContentType = detect_content_type(Filename),
    validate(#hikyaku_attachment{
        filename = iolist_to_binary(Filename),
        content_type = ContentType,
        path = Path,
        disposition = inline,
        cid = Cid
    }).

-doc """
Checks that an attachment is safe to put into MIME and form-data headers.

The content type must be a bare `type/subtype` made of RFC 9110 token
characters (no parameters), the filename must not contain control
characters, and a content id must be printable ASCII without `<` or `>`.
Returns the attachment, or raises `{invalid_attachment, Field, Value}`.
All builders in this module and `hikyaku_email:attachment/2` call it.
""".
-spec validate(#hikyaku_attachment{}) -> #hikyaku_attachment{}.
validate(
    #hikyaku_attachment{filename = Filename, content_type = ContentType, cid = Cid} = Att
) ->
    check(content_type, ContentType, ?CONTENT_TYPE_RE),
    check(filename, Filename, ~"\\A[^\\x00-\\x1f\\x7f]+\\z"),
    case Cid of
        undefined -> ok;
        _ -> check(cid, Cid, ~"\\A[!-;=?-~]+\\z")
    end,
    Att.

%%% Internal

check(Field, Value, Re) ->
    case re:run(Value, Re) of
        nomatch -> error({invalid_attachment, Field, Value});
        {match, _} -> ok
    end.

apply_opts(Att, Opts) ->
    Att1 =
        case maps:find(content_type, Opts) of
            {ok, CT} -> Att#hikyaku_attachment{content_type = CT};
            error -> Att
        end,
    Att2 =
        case maps:find(filename, Opts) of
            {ok, FN} -> Att1#hikyaku_attachment{filename = FN};
            error -> Att1
        end,
    Att2.

-spec detect_content_type(binary() | string()) -> binary().
detect_content_type(Filename) ->
    Ext = string:lowercase(filename:extension(Filename)),
    case iolist_to_binary(Ext) of
        <<".txt">> ->
            <<"text/plain">>;
        <<".html">> ->
            <<"text/html">>;
        <<".htm">> ->
            <<"text/html">>;
        <<".css">> ->
            <<"text/css">>;
        <<".csv">> ->
            <<"text/csv">>;
        <<".xml">> ->
            <<"application/xml">>;
        <<".json">> ->
            <<"application/json">>;
        <<".pdf">> ->
            <<"application/pdf">>;
        <<".zip">> ->
            <<"application/zip">>;
        <<".gz">> ->
            <<"application/gzip">>;
        <<".tar">> ->
            <<"application/x-tar">>;
        <<".doc">> ->
            <<"application/msword">>;
        <<".docx">> ->
            <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>;
        <<".xls">> ->
            <<"application/vnd.ms-excel">>;
        <<".xlsx">> ->
            <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>;
        <<".png">> ->
            <<"image/png">>;
        <<".jpg">> ->
            <<"image/jpeg">>;
        <<".jpeg">> ->
            <<"image/jpeg">>;
        <<".gif">> ->
            <<"image/gif">>;
        <<".svg">> ->
            <<"image/svg+xml">>;
        <<".webp">> ->
            <<"image/webp">>;
        <<".ico">> ->
            <<"image/x-icon">>;
        <<".mp3">> ->
            <<"audio/mpeg">>;
        <<".wav">> ->
            <<"audio/wav">>;
        <<".mp4">> ->
            <<"video/mp4">>;
        <<".webm">> ->
            <<"video/webm">>;
        <<".erl">> ->
            <<"text/plain">>;
        <<".ex">> ->
            <<"text/plain">>;
        _ ->
            <<"application/octet-stream">>
    end.
