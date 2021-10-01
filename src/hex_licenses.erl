%% @doc
%% Functions for determine license validatity.

-module(hex_licenses).

-define(CANONICAL_LICENSES, load_licenses()).

-export([licenses/0]).
-export([invalid_licenses/1]).

load_licenses() ->
    [CanonicalLicenses] = licenses(),
    CanonicalLicenses.

licenses() ->
    {ok, Contents} = file:consult(code:priv_dir(hex_core) ++ "/licenses.erlterm"),
    Contents.

invalid_licenses(UserLicenseList) ->
    CanonicalLicenseIDs = license_ids_from_license_list(?CANONICAL_LICENSES, gb_sets:new()),
    InvalidLicenses =
        lists:filter(fun(UserLicense) -> not gb_sets:is_element(UserLicense, CanonicalLicenseIDs)
                     end,
                     UserLicenseList),
    InvalidLicenses.

license_ids_from_license_list([], Acc) ->
    Acc;
license_ids_from_license_list([#{license_id := LicenseID} | T], Acc) ->
    NewAcc = gb_sets:add_element(LicenseID, Acc),
    license_ids_from_license_list(T, NewAcc).
