procedure t is

   e : exception;

   type newint is new integer
   affirm
     raise e when newint /= 0;
   end affirm;

   procedure testproc( n : newint ) is
   begin
     ? n;
   end testproc;

begin
  testproc(1);
end t;

