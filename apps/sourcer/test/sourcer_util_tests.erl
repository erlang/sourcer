-module(sourcer_util_tests).

-include_lib("eunit/include/eunit.hrl").

uri_test_() ->
    Data = [
        {<<"file:///place/foo/bar.baz">>, "/place/foo/bar.baz"},
        %{<<"file:///c%3A/path/to/file/d%C3%BCr%C3%BCm%20d%C3%B6ner.php">>,
        %"c:\\path\\to\\file\\dürüm döner.php"
        %},
        {<<"file:///c%3A/path/to/file/dürüm%20döner.php"/utf8>>,
        "c:\\path\\to\\file\\dürüm döner.php"
        },
        {<<"file:///c%3A/foo/bar.baz">>, "c:\\foo\\bar.baz"}
    ],
    [
        [
            ?_assertEqual(Path, sourcer_util:uri_to_path(Uri)),
            ?_assertEqual(Uri, sourcer_util:path_to_uri(Path))
        ]
        || {Uri, Path} <- Data
    ].

uri_2_test_() ->
    Data = [
        {<<"file:///c:/foo/bar.baz">>, "c:\\foo\\bar.baz"}
    ],
    [
        [
            ?_assertEqual(Path, sourcer_util:uri_to_path(Uri))
        ]
        || {Uri, Path} <- Data
    ].
