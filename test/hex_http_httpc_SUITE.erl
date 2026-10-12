-module(hex_http_httpc_SUITE).

-compile([export_all]).

all() ->
    [
        request_to_file_returns_headers,
        request_to_file_streams_large_body,
        request_to_file_returns_error_status,
        http_options_without_cacerts,
        http_options_with_cacerts,
        request_rejects_untrusted_certificate,
        request_to_file_rejects_untrusted_certificate,
        request_uses_ssl_option
    ].

init_per_suite(Config) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    Config.

end_per_suite(_Config) ->
    ok.

request_to_file_returns_headers(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Body = <<"hello world">>,
    Response = [
        "HTTP/1.1 200 OK\r\n"
        "content-length: ",
        integer_to_list(byte_size(Body)),
        "\r\n"
        "etag: \"abc123\"\r\n"
        "\r\n",
        Body
    ],

    {ok, ListenSock} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(ListenSock),

    Self = self(),
    spawn_link(fun() ->
        {ok, Sock} = gen_tcp:accept(ListenSock),
        {ok, _} = gen_tcp:recv(Sock, 0, 5000),
        gen_tcp:send(Sock, Response),
        gen_tcp:close(Sock),
        Self ! server_done
    end),

    Filename = filename:join(PrivDir, "download_200"),
    URI = iolist_to_binary(["http://localhost:", integer_to_list(Port), "/test"]),

    {ok, {200, Headers}} =
        hex_http_httpc:request_to_file(get, URI, #{}, undefined, Filename, #{}),

    <<"\"abc123\"">> = maps:get(<<"etag">>, Headers),
    {ok, Body} = file:read_file(Filename),

    receive
        server_done -> ok
    after 5000 ->
        error(server_timeout)
    end,
    gen_tcp:close(ListenSock).

%% Send a body large enough that httpc delivers it as multiple stream
%% messages to verify the receive loop reassembles the file correctly.
request_to_file_streams_large_body(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    %% 1 MB body — well above httpc's internal chunk size
    Body = binary:copy(<<"x">>, 1024 * 1024),
    Response = [
        "HTTP/1.1 200 OK\r\n"
        "content-length: ",
        integer_to_list(byte_size(Body)),
        "\r\n\r\n",
        Body
    ],

    {ok, ListenSock} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(ListenSock),

    Self = self(),
    spawn_link(fun() ->
        {ok, Sock} = gen_tcp:accept(ListenSock),
        {ok, _} = gen_tcp:recv(Sock, 0, 5000),
        gen_tcp:send(Sock, Response),
        gen_tcp:close(Sock),
        Self ! server_done
    end),

    Filename = filename:join(PrivDir, "download_large"),
    URI = iolist_to_binary(["http://localhost:", integer_to_list(Port), "/test"]),

    {ok, {200, _Headers}} =
        hex_http_httpc:request_to_file(get, URI, #{}, undefined, Filename, #{}),

    {ok, Body} = file:read_file(Filename),

    receive
        server_done -> ok
    after 5000 ->
        error(server_timeout)
    end,
    gen_tcp:close(ListenSock).

%% httpc async streaming only streams 2xx responses as messages.
%% Non-2xx responses return the normal response tuple with the real
%% status code and headers.
request_to_file_returns_error_status(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    assert_error_status(
        404, PrivDir, "HTTP/1.1 404 Not Found\r\ncontent-length: 9\r\n\r\nnot found"
    ),
    assert_error_status(
        403, PrivDir, "HTTP/1.1 403 Forbidden\r\ncontent-length: 9\r\n\r\nforbidden"
    ),
    assert_error_status(
        500, PrivDir, "HTTP/1.1 500 Internal Server Error\r\ncontent-length: 6\r\n\r\nerror!"
    ),
    ok.

assert_error_status(ExpectedStatus, PrivDir, Response) ->
    {ok, ListenSock} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(ListenSock),

    Self = self(),
    spawn_link(fun() ->
        {ok, Sock} = gen_tcp:accept(ListenSock),
        {ok, _} = gen_tcp:recv(Sock, 0, 5000),
        gen_tcp:send(Sock, Response),
        gen_tcp:close(Sock),
        Self ! server_done
    end),

    Filename = filename:join(PrivDir, "download_" ++ integer_to_list(ExpectedStatus)),
    URI = iolist_to_binary(["http://localhost:", integer_to_list(Port), "/test"]),

    {ok, {ExpectedStatus, _Headers}} =
        hex_http_httpc:request_to_file(get, URI, #{}, undefined, Filename, #{}),

    receive
        server_done -> ok
    after 5000 ->
        error(server_timeout)
    end,
    gen_tcp:close(ListenSock).

http_options_without_cacerts(_Config) ->
    Fail = fun() -> error(undef) end,
    lists:foreach(
        fun({URI, HTTPOptions}) ->
            {error, {cacerts_unavailable, undef}} =
                hex_http_httpc:http_options(URI, #{http_options => HTTPOptions}, Fail)
        end,
        [
            {<<"https://repo.hex.pm">>, []},
            {<<"HTTPS://repo.hex.pm">>, []},
            {<<"hTtPs://repo.hex.pm">>, []},
            {<<"https://repo.hex.pm">>, [ssl]},
            {<<"https://repo.hex.pm">>, [{ssl, undefined}]}
        ]
    ),
    SSLOpts = [{verify, verify_peer}, {cacertfile, "ca.pem"}],
    {ok, [{ssl, SSLOpts}]} =
        hex_http_httpc:http_options(
            <<"https://repo.hex.pm">>, #{http_options => [{ssl, SSLOpts}]}, Fail
        ),
    {ok, []} = hex_http_httpc:http_options(<<"http://localhost">>, #{}, Fail),
    {ok, []} = hex_http_httpc:http_options(<<"HTTP://localhost">>, #{}, Fail),
    ok.

http_options_with_cacerts(_Config) ->
    GetCACerts = fun() -> [<<"cert">>] end,
    {ok, [{ssl, SSLOpts}, {timeout, 1000}]} =
        hex_http_httpc:http_options(
            <<"https://repo.hex.pm">>, #{http_options => [{timeout, 1000}]}, GetCACerts
        ),
    verify_peer = proplists:get_value(verify, SSLOpts),
    [<<"cert">>] = proplists:get_value(cacerts, SSLOpts),
    {ok, [{ssl, SSLOpts}, {timeout, 1000}]} =
        hex_http_httpc:http_options(
            <<"https://repo.hex.pm">>, #{http_options => [ssl, {timeout, 1000}]}, GetCACerts
        ),
    ok.

%% The server's certificate is issued by a CA that isn't in the
%% operating system's CA store, so the default options reject it. On
%% OTP 24 the request fails before connecting.
request_rejects_untrusted_certificate(_Config) ->
    #{server_config := ServerOpts} = tls_test_data(),
    lists:foreach(
        fun({Scheme, HTTPOptions}) ->
            {ListenSock, Port} = tls_server(ServerOpts),
            {error, _} =
                hex_http_httpc:request(
                    get, tls_uri(Scheme, Port), #{}, undefined, #{http_options => HTTPOptions}
                ),
            no_request = tls_server_result(ListenSock)
        end,
        untrusted_cases()
    ).

request_to_file_rejects_untrusted_certificate(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Filename = filename:join(PrivDir, "download_untrusted"),
    #{server_config := ServerOpts} = tls_test_data(),
    lists:foreach(
        fun({Scheme, HTTPOptions}) ->
            {ListenSock, Port} = tls_server(ServerOpts),
            {error, _} =
                hex_http_httpc:request_to_file(
                    get,
                    tls_uri(Scheme, Port),
                    #{},
                    undefined,
                    Filename,
                    #{http_options => HTTPOptions}
                ),
            no_request = tls_server_result(ListenSock),
            false = filelib:is_file(Filename)
        end,
        untrusted_cases()
    ).

request_uses_ssl_option(_Config) ->
    #{server_config := ServerOpts, client_config := ClientOpts} = tls_test_data(),
    {ListenSock, Port} = tls_server(ServerOpts),
    SSLOpts = [
        {verify, verify_peer},
        {cacerts, proplists:get_value(cacerts, ClientOpts)},
        {server_name_indication, disable}
    ],
    {ok, {200, _Headers, <<"ok">>}} =
        hex_http_httpc:request(
            get, tls_uri(<<"https">>, Port), #{}, undefined, #{http_options => [{ssl, SSLOpts}]}
        ),
    request_served = tls_server_result(ListenSock).

%% httpc reads the scheme without regard to case and ignores a bare ssl atom
untrusted_cases() ->
    [{<<"https">>, []}, {<<"HTTPS">>, []}, {<<"https">>, [ssl]}].

tls_uri(Scheme, Port) ->
    iolist_to_binary([Scheme, "://localhost:", integer_to_list(Port), "/test"]).

tls_test_data() ->
    KeyOpts = [{key, {namedCurve, secp256r1}}, {digest, sha256}],
    Chain = #{root => KeyOpts, intermediates => [], peer => KeyOpts},
    public_key:pkix_test_data(#{server_chain => Chain, client_chain => Chain}).

tls_server(ServerOpts) ->
    {ok, ListenSock} = ssl:listen(0, [binary, {active, false}, {reuseaddr, true} | ServerOpts]),
    {ok, {_, Port}} = ssl:sockname(ListenSock),
    Self = self(),
    spawn_link(fun() ->
        Result =
            case ssl:transport_accept(ListenSock) of
                {ok, Sock} ->
                    case ssl:handshake(Sock, 5000) of
                        {ok, TLSSock} ->
                            {ok, _} = ssl:recv(TLSSock, 0, 5000),
                            ok = ssl:send(
                                TLSSock, "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok"
                            ),
                            ssl:close(TLSSock),
                            request_served;
                        {error, _} ->
                            no_request
                    end;
                {error, _} ->
                    no_request
            end,
        Self ! {tls_server, Result}
    end),
    {ListenSock, Port}.

tls_server_result(ListenSock) ->
    ssl:close(ListenSock),
    receive
        {tls_server, Result} -> Result
    after 5000 ->
        error(server_timeout)
    end.
