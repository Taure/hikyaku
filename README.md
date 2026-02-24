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
-include_lib("hikyaku/include/hikyaku.hrl").

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

## Adapters

| Adapter | Description | Config |
|---|---|---|
| `hikyaku_adapter_smtp` | SMTP via gen_smtp | `relay`, `port`, `username`, `password`, `ssl`, `tls` |
| `hikyaku_adapter_sendgrid` | SendGrid v3 API | `api_key`, `http_client` |
| `hikyaku_adapter_mailgun` | Mailgun Messages API | `api_key`, `domain`, `base_url`, `http_client` |
| `hikyaku_adapter_ses` | Amazon SES v2 API | `access_key`, `secret_key`, `region`, `http_client` |
| `hikyaku_adapter_logger` | Logs emails via logger | `level` |
| `hikyaku_adapter_test` | Sends to a process | `pid` |

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

SES uses the v2 JSON API for simple emails and automatically falls back to raw MIME encoding when attachments are present. Authentication uses AWS Signature V4 — no external dependencies required beyond OTP `crypto`.

## Testing

```erlang
-include_lib("hikyaku/include/hikyaku.hrl").
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
            ?assertEqual(<<"Test">>, Received#hikyaku_email.subject)
    after 1000 ->
        error(timeout)
    end.
```

## Requirements

- Erlang/OTP 27+
