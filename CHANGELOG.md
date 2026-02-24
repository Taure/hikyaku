# Changelog

## v0.2.0

### Enhancements

  - Add Mailgun adapter (`hikyaku_adapter_mailgun`) using the Messages API with multipart/form-data encoding
  - Add Amazon SES v2 adapter (`hikyaku_adapter_ses`) with JSON API and raw MIME fallback for attachments
  - Add AWS Signature V4 signing module (`hikyaku_aws_auth`) using only OTP `crypto`
  - Add multipart form-data encoder (`hikyaku_multipart`) and URL percent-encoding utility (`hikyaku_url`)

### Fixes

  - Fix `format_address_list/1` in SMTP adapter to return a flat binary instead of an iolist

## v0.1.0

  - Initial release with SMTP, SendGrid, Logger, and Test adapters
  - Composable email builder API
  - Attachment support with MIME type detection
  - HTTP abstraction layer for testability
