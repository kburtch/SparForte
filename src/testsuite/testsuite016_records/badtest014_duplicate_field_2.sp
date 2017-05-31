i : integer; -- i is an existing identifier
type somerec is record
    i : integer;
    i : integer; -- should not be twice
end record;

