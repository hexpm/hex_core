-module(hex_http_httpc_SUITE).

-compile([export_all]).

all() ->
    [
        request_to_file_returns_headers,
        request_to_file_streams_large_body,
        request_to_file_returns_error_status
    ].

init_per_suite(Config) ->
    application:ensure_all_started(inets),
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
