# Hikyaku

Email delivery library for Erlang — composable email builder with swappable adapter backends.

## Features

- **Builder** — functional, chainable email construction with address normalization
- **Attachments** — from file path, binary data, or inline with content-id
- **Mailer** — behaviour-based mailer modules with validation on delivery
- **Adapters** — SMTP, SendGrid, Mailgun, Amazon SES, logger, and test
- **Content Types** — automatic MIME type detection for attachments

## Quick Start

### Define a Mailer

```erlang
-module(my_mailer).
-behaviour(hikyaku_mailer).

-export([config/0, deliver/1]).

config() ->
    #{
        adapter => hikyaku_adapter_smtp,
        relay => <<"smtp.example.com">>,
        port => 587,
        username => <<"user@example.com">>,
        password => <<"secret">>,
        tls => always
    }.

deliver(Email) ->
    hikyaku_mailer:deliver(?MODULE, Email).
```

### Build & Send

```erlang
Email = hikyaku_email:new(),
Email1 = hikyaku_email:from(Email, {<<"Alice">>, <<"alice@example.com">>}),
Email2 = hikyaku_email:to(Email1, <<"bob@example.com">>),
Email3 = hikyaku_email:subject(Email2, <<"Hello!">>),
Email4 = hikyaku_email:text_body(Email3, <<"Hi Bob">>),
Email5 = hikyaku_email:html_body(Email4, <<"<b>Hi Bob</b>">>),

{ok, _} = my_mailer:deliver(Email5).
```

Addresses accept a binary (`<<"bob@example.com">>`), a `{Name, Address}` tuple, or a `#hikyaku_address{}` record.

Use `to/2`, `cc/2`, `bcc/2` to append recipients, or `put_to/2`, `put_cc/2`, `put_bcc/2` to replace the entire list.

Display names containing RFC 5322 specials (such as `,` or `<`) are sent as quoted strings. Addresses and names with control characters, and addresses containing whitespace, `<`, `>` or `,`, raise `{invalid_address, _}`.

Custom headers are set with `header/3`. The name must be printable ASCII without `:` (at most 76 characters) and the value must not contain CR or LF, otherwise `{invalid_header, Name}` is raised. Headers hikyaku derives from other fields (`From`, `To`, `Cc`, `Bcc`, `Reply-To`, `Subject`, `Content-Type`, `MIME-Version`, `Content-Transfer-Encoding`) raise `{reserved_header, Name}`.

### Attachments

```erlang
%% From file path (content type auto-detected)
Pdf = hikyaku_attachment:from_path(<<"/tmp/report.pdf">>),

%% From binary data
Csv = hikyaku_attachment:from_data(<<"data.csv">>, CsvBin),

%% Inline image with content-id
Logo = hikyaku_attachment:inline(<<"logo">>, <<"/tmp/logo.png">>),

Email6 = hikyaku_email:attachment(Email5, Pdf),
Email7 = hikyaku_email:attachment(Email6, Logo).
```

`from_path/2` and `from_data/3` accept an options map: `#{content_type => binary(), filename => binary()}`.

The content type must be a bare `type/subtype` (no parameters), the filename must not contain control characters, and an inline content id must be printable ASCII without `<` or `>`. Anything else raises `{invalid_attachment, Field, Value}`, both from the builders and from `hikyaku_email:attachment/2`.

### Reading an Email

Each setter has a matching one-argument getter, so a built or delivered email can be read without including the record header:

```erlang
<<"Hello!">> = hikyaku_email:subject(Email5),
<<"Hi Bob">> = hikyaku_email:text_body(Email5),
[_Bob] = hikyaku_email:to(Email5).
```

Getters: `subject/1`, `from/1`, `to/1`, `cc/1`, `bcc/1`, `reply_to/1`, `text_body/1`, `html_body/1`, `attachments/1`, `headers/1`, `provider_options/1`. Addresses are returned as `#hikyaku_address{}` records and attachments as `#hikyaku_attachment{}` records (from `hikyaku/include/hikyaku.hrl`).

## Adapters

| Adapter | Description | Config |
|---|---|---|
| `hikyaku_adapter_smtp` | SMTP via gen_smtp | `relay`, `port`, `username`, `password`, `ssl`, `tls`, `tls_options`, `no_mx_lookups` |
| `hikyaku_adapter_sendgrid` | SendGrid v3 API | `api_key`, `http_client` |
| `hikyaku_adapter_mailgun` | Mailgun Messages API | `api_key`, `domain`, `base_url`, `http_client` |
| `hikyaku_adapter_ses` | Amazon SES v2 API | `access_key`, `secret_key`, `region`, `http_client` |
| `hikyaku_adapter_logger` | Logs emails via logger | `level` |
| `hikyaku_adapter_test` | Sends to a process | `pid` |

### SMTP

```erlang
config() ->
    #{
        adapter => hikyaku_adapter_smtp,
        relay => <<"smtp.example.com">>,
        port => 587,                      %% default 587; use 465 with ssl => true
        username => <<"user@example.com">>,
        password => <<"secret">>,
        tls => always                     %% always | if_available | never
    }.
```

- `tls` controls STARTTLS. It defaults to `always`, so neither credentials nor the message are sent over an unencrypted connection. The exceptions are `ssl => true` (already encrypted) and a loopback relay (`localhost`, `127.0.0.0/8`, `::1`) without `username`, where it defaults to `if_available`. Setting `tls => if_available` elsewhere is an explicit downgrade: an on-path attacker can strip STARTTLS and read the session.
- `ssl => true` connects with implicit TLS (port 465) instead of STARTTLS.
- Every TLS connection, STARTTLS or implicit, verifies the relay certificate against the OS trust store (`public_key:cacerts_get()`) and checks that it is valid for `relay`, which is also sent as SNI.
- `tls_options` replaces the default ssl client options, for example to trust a private CA:

  ```erlang
  tls_options => [{verify, verify_peer}, {cacerts, MyCaCerts}]
  ```

  `relay` is still sent as SNI unless the list sets `server_name_indication`, and `{depth, 3}` is added unless the list sets `depth`, so a chain with intermediates verifies.
- gen_smtp looks up MX records for `relay` and connects to those hosts if any exist. Set `no_mx_lookups => true` to always connect to `relay` itself. MX lookups are always skipped when `relay` is an IP address or `tls_options` sets `{server_name_indication, disable}`, because the certificate check then has no name to tell the relay from a host a spoofed MX answer points at.
- Unless the email sets its own `Message-ID` header, one is generated from random bytes and the sender's domain, so the sending host's name does not leak.

### SendGrid

```erlang
config() ->
    #{
        adapter => hikyaku_adapter_sendgrid,
        api_key => <<"SG.xxx">>
    }.
```

Attachments (including inline parts with a content id) and custom headers are sent through the v3 `mail/send` API.

### Mailgun

```erlang
config() ->
    #{
        adapter => hikyaku_adapter_mailgun,
        api_key => <<"key-xxx">>,
        domain => <<"mg.example.com">>,
        base_url => <<"https://api.eu.mailgun.net">>  %% optional, defaults to US region
    }.
```

### Amazon SES

```erlang
config() ->
    #{
        adapter => hikyaku_adapter_ses,
        access_key => <<"AKIA...">>,
        secret_key => <<"...">>,
        region => <<"us-east-1">>
    }.
```

SES uses the v2 JSON API for simple emails and automatically falls back to raw MIME encoding when attachments or custom headers are present. Authentication uses AWS Signature V4 — no external dependencies required beyond OTP `crypto`.

## Testing

```erlang
-include_lib("eunit/include/eunit.hrl").

-behaviour(hikyaku_mailer).

config() ->
    #{adapter => hikyaku_adapter_test, pid => self()}.

send_test() ->
    Email = hikyaku_email:to(
        hikyaku_email:from(
            hikyaku_email:subject(hikyaku_email:new(), <<"Test">>),
            <<"sender@test.com">>
        ),
        <<"recipient@test.com">>
    ),
    {ok, _} = hikyaku_mailer:deliver(?MODULE, Email),
    receive
        {hikyaku_email, Received} ->
            ?assertEqual(<<"Test">>, hikyaku_email:subject(Received))
    after 1000 ->
        error(timeout)
    end.
```

## Requirements

- Erlang/OTP 27+
