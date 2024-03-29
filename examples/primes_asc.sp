#!/usr/local/bin/spar

pragma annotate( summary, "primes_asc" );
pragma annotate( description, "Generate and show all primes with strictly ascending decimal digits" );
pragma annotate( description, "Translation of Pascal" );
pragma annotate( see_also, "https://rosettacode.org/wiki/Ascending_primes" );
pragma annotate( author, "Ken O. Burtch" );

pragma software_model( nonstandard );
pragma restriction( no_external_commands );

procedure primes_asc is
  maxsize : constant natural := 1000;

  queue : array(1..maxsize) of natural;
  primes: array(1..maxsize) of natural;

  b : natural;
  e : natural;
  n : natural;
  v : natural;

function is_prime(num: integer) return boolean is
    found : boolean;
    num_root : natural;
    k : natural;
  begin
    if num = 2 then
      found;
    elsif (num = 1) or (num mod 2 = 0) then
      found := false;
    else
      num_root := numerics.truncation(numerics.sqrt(num));
      found;
      k := 3;
      while found and (k <= num_root) loop
        if num mod k = 0 then
          found := false;
        else
          k := @ + 2;
        end if;
      end loop;
    end if;
    return found;
  end is_prime;

begin
  b := 1;
  e := 1;
  n := 0;

  for k in 1..9 loop
    queue(e) := k;
    e := e + 1;
  end loop;

  while b < e loop
    v := queue(b);
    b := @ + 1;
    if is_prime(v) then
      n := @ + 1;
      primes(n) := v;
    end if;

    for k in v mod 10 + 1..9 loop
      queue(e) := v * 10 + k;
      e := @ + 1;
    end loop;
  end loop;

  for k in 1..n loop
    put(primes(k), "ZZZZZZZZ9");
    if k mod 8 = 0 then
       new_line;
    end if;
  end loop;
  new_line;
end primes_asc;

-- VIM editor formatting instructions
-- vim: ft=spar

