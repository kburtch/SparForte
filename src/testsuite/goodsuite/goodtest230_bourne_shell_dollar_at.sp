result : string;

result := `../../spar dollar_at_sign.sp a "b" "c d"`;
pragma assert ( numerics.value(result) = 3 );

