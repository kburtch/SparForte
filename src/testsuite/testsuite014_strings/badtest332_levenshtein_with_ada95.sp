pragma ada_95;
s : string := "abc";
i : integer;
i := strings.levenshtein( s, s ); -- not allowed with pragma ada95

