pragma ada_95;
s : string := "abc";
s := strings.replace_all( s, s, s ); -- not allowed with pragma ada95

