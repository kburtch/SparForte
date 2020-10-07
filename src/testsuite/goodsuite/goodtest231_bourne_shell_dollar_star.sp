result : string;

result := `../../spar dollar_star.sp a "b" "c d"`;
pragma assert ( numerics.value(result) = 4 );

