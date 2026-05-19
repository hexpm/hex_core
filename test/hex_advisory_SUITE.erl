-module(hex_advisory_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        identity_when_no_aliases,
        groups_advisories_sharing_cve_alias,
        prefers_eef_over_ghsa_over_nvd,
        breaks_same_source_ties_by_id_ascending,
        attaches_osv_url_only_to_alias_ids_that_are_group_members
    ].

identity_when_no_aliases(_Config) ->
    A = base_advisory(<<"GHSA-aaaa-1111-bbbb">>, <<"single">>, []),
    [Result] = hex_advisory:group_for_display([A]),
    ?assertEqual(<<"GHSA-aaaa-1111-bbbb">>, maps:get(id, Result)),
    ?assertEqual([], maps:get(aliases, Result)),
    ok.

groups_advisories_sharing_cve_alias(_Config) ->
    A = base_advisory(<<"GHSA-aaaa-1111-bbbb">>, <<"A">>, [<<"CVE-2026-0001">>]),
    B = base_advisory(<<"GHSA-cccc-2222-dddd">>, <<"B">>, [<<"CVE-2026-0001">>]),
    [Result] = hex_advisory:group_for_display([A, B]),
    %% Same source priority (GHSA-), tie-break by id ascending → A wins
    ?assertEqual(<<"GHSA-aaaa-1111-bbbb">>, maps:get(id, Result)),
    AliasIds = [maps:get(id, Alias) || Alias <- maps:get(aliases, Result)],
    ?assertEqual(
        lists:sort([<<"CVE-2026-0001">>, <<"GHSA-cccc-2222-dddd">>]),
        lists:sort(AliasIds)
    ),
    ok.

prefers_eef_over_ghsa_over_nvd(_Config) ->
    Eef = base_advisory(<<"EEF-CVE-2026-9">>, <<"E">>, [<<"CVE-2026-9">>]),
    Ghsa = base_advisory(<<"GHSA-zzzz-9999-zzzz">>, <<"G">>, [<<"CVE-2026-9">>]),
    Nvd = base_advisory(<<"NVD-CVE-2026-9">>, <<"N">>, [<<"CVE-2026-9">>]),
    [Result] = hex_advisory:group_for_display([Nvd, Ghsa, Eef]),
    ?assertEqual(<<"EEF-CVE-2026-9">>, maps:get(id, Result)),
    ok.

breaks_same_source_ties_by_id_ascending(_Config) ->
    A = base_advisory(<<"OTHER-2026-0002">>, <<"A">>, [<<"CVE-2026-0002">>]),
    B = base_advisory(<<"OTHER-2026-0001">>, <<"B">>, [<<"CVE-2026-0002">>]),
    [Result] = hex_advisory:group_for_display([A, B]),
    ?assertEqual(<<"OTHER-2026-0001">>, maps:get(id, Result)),
    ok.

attaches_osv_url_only_to_alias_ids_that_are_group_members(_Config) ->
    A = base_advisory(<<"GHSA-aaaa-1111-bbbb">>, <<"A">>, [<<"CVE-2026-0001">>]),
    B = base_advisory(<<"GHSA-cccc-2222-dddd">>, <<"B">>, [<<"CVE-2026-0001">>]),
    [Result] = hex_advisory:group_for_display([A, B]),
    ByName = maps:from_list([{maps:get(id, X), X} || X <- maps:get(aliases, Result)]),
    %% GHSA-cccc-2222-dddd is itself an advisory in the group → has osv.dev url
    Ghsa = maps:get(<<"GHSA-cccc-2222-dddd">>, ByName),
    ?assertEqual(
        <<"https://osv.dev/vulnerability/GHSA-cccc-2222-dddd">>,
        maps:get(url, Ghsa)
    ),
    %% CVE-2026-0001 was only an alias string → no url
    Cve = maps:get(<<"CVE-2026-0001">>, ByName),
    ?assertEqual(undefined, maps:get(url, Cve)),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

base_advisory(Id, Summary, Aliases) ->
    #{
        id => Id,
        summary => Summary,
        html_url => <<"https://osv.dev/vulnerability/", Id/binary>>,
        api_url => <<"https://api.osv.dev/v1/vulns/", Id/binary>>,
        aliases => Aliases
    }.
