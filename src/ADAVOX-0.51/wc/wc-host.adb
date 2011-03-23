-- wc-host.adb (generated by hostconst.c)
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License
--
-- WARNING: This file was generated by hostconst.c!
package body WC.Host is

   function Set_Scheduler_Policy(Policy : Scheduler_Policy; Priority : Integer) return Errno_Type
   is
      function C_Set_Sched(Policy : Scheduler_Policy; Priority : Integer) return Errno_Type;
      pragma import(C,C_Set_Sched,"set_sched_policy");
   begin
      return C_Set_Sched(Policy,Priority);
   end Set_Scheduler_Policy;

   function Get_Scheduler_Priority(Policy : Scheduler_Policy; Max : Boolean := True) return Integer
   is
      function C_get_max(Policy : Scheduler_Policy) return Integer;
      pragma import(C,C_get_max,"sched_get_priority_max");
      function C_get_min(Policy : Scheduler_Policy) return Integer;
      pragma import(C,C_get_min,"sched_get_priority_min");
   begin

      if Max then
         return C_get_max(Policy);
      else
         return C_get_min(Policy);
      end if;
   end Get_Scheduler_Priority;

   procedure squash_root is
      procedure C_squash_root;
      pragma import(C,C_squash_root,"squash_root");
   begin
      C_squash_root;
   end Squash_Root;

end WC.Host;