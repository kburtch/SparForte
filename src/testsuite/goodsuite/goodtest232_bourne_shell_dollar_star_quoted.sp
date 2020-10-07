result : string;

result := `../../spar dollar_star_quoted.sp a "b" "c d"`;
pragma assert ( numerics.value(result) = 1 );

