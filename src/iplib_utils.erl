
-module(iplib_utils).

-include("defined.hrl").

-export([ip2long/1, priv_dir/1]).

ip2long(Address) when is_integer(Address) ->
    {ok, Address};
ip2long(Address) when is_list(Address) ->
    try
        case address_fast(lists:flatten(Address), 0, 24) of
        N when is_integer(N) ->
            {ok, N};
        _ ->
            case inet_parse:address(lists:flatten(Address)) of
            {ok, Tuple} ->
                ip2long(Tuple);
            Error ->
                {error, Error}
            end
        end
    catch _:Err ->
        {error, Err}
    end;
ip2long({B3, B2, B1, B0}) when is_integer(B3),is_integer(B2),is_integer(B1),is_integer(B0) ->
    {ok, (B3 bsl 24) bor (B2 bsl 16) bor (B1 bsl 8) bor B0};
ip2long({W7, W6, W5, W4, W3, W2, W1, W0}) when
is_integer(W7),is_integer(W6),is_integer(W5),is_integer(W4),is_integer(W3),is_integer(W2),is_integer(W1),is_integer(W0)->
    {ok, (W7 bsl 112) bor (W6 bsl 96) bor (W5 bsl 80) bor (W4 bsl 64) bor
	(W3 bsl 48) bor (W2 bsl 32) bor (W1 bsl 16) bor W0};
ip2long(_) ->
    {error, badmatch}.

address_fast([N2, N1, N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case list_to_integer([N2, N1, N0]) of
	N when N =< 255 ->
	    address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast([N1, N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case list_to_integer([N1, N0]) of
	N when N =< 255 ->
	    address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast([N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case N0 - $0 of
	N when N =< 255 ->
	    address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast(L=[_N2, _N1, _N0], Num, 0) ->
    case list_to_integer(L) of
	N when N =< 255 ->
	    Num bor N
    end;
address_fast(L=[_N1, _N0], Num, 0) ->
    case list_to_integer(L) of
	N when N =< 255 ->
	    Num bor N
    end;
address_fast([N0], Num, 0) ->
    case N0 - $0 of
	N when N =< 255 ->
	    Num bor N
    end.

priv_dir(App)->
    case code:priv_dir(App) of
        {error,bad_name} ->
            AppStr = atom_to_list(App),
            Path = code:where_is_file(AppStr++".app"),
            N = string:len(Path) - string:len(AppStr) - 5,
            string:substr(Path, 1, N) ++ "/../priv";
        Path when is_list(Path) -> Path
    end.
