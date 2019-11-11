# CHANGELOG

## 0.6.2 (2019-11-11)

* Gracefully handle HTTP errors

## 0.6.1 (2019-07-30)

* Fix bug when publishing with configuration that does not have `repo_organization` key set

## 0.6.0 (2019-07-27)

* **Backwards-incompatible**: Change successful return value of `hex_tarball:create/2` from
  `{ok, {Tarball, Checksum}}` to `{ok, Map}`. Currently, `Map` has `tarball`, `inner_tarball`,
  and `outer_tarball` keys
* Rename existing tarball checksum to inner checksum and deprecate it
* Add tarball outer checksum
* Do not allow comments in `safe_erl_term`
* Forward compatible tarball contents by not failing on unknown files

## 0.5.1 (2019-07-30)

* Fix bug when publishing with configuration that does not have `repo_organization` key set

## 0.5.0 (2019-02-26)

* Fetch package data from proper repository organization and don't verify origin if configured
* **Backwards-incompatible**: Make `hex_api` module private
* **Backwards-incompatible**: `organization` config key has been replaced with `api_organization`
  and `api_repository` keys
* Add `repo_organization` config key
* `vendor.sh`: Add missing `hex_pb_*.erl` files
* `vendor.sh`: Fix replacing `hex_core`

## 0.4.0 (2018-12-30)

* Revert swich from proto2 to proto3
* Verify authenticity of signed payload
* Add `repo_verify_origin` config option

## 0.3.0 (2018-12-14)

* Swich from proto2 to proto3
* Fix `hex_http:body` type
* Fix `hex_http:request` spec

## 0.2.1 (2018-12-13)

* Always return the whole response tuple from requests
* Guess rebar3 as build tool over rebar
* Return compressed tarball from hex_tarball:create_docs/1
* Add package identity to registry records

## 0.2.0 (2018-09-01)

* Accept `{filename(), filename()}` in files list for tarballs
* Support httpc profile through adapter configs
* Update generated protobuf files with OTP 21 compatibility

## 0.1.1 (2018-08-11)

* Use `hex_core` project name in user agent string

## 0.1.0 (2018-08-08)

* First release
