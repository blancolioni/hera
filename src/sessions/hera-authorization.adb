with Ada.Strings.Unbounded;

with Hera.Corporations;

package body Hera.Authorization is

   type Authorized_Account is
     new Hera.UI.UI_Account_Interface with
      record
         User_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Is_Administrator
     (Account : Authorized_Account)
      return Boolean
   is (True);

   overriding function User_Name
     (Account : Authorized_Account)
      return String
   is (Ada.Strings.Unbounded.To_String (Account.User_Name));

   -----------
   -- Login --
   -----------

   function Login
     (User_Name : String;
      Password  : String)
      return Hera.UI.UI_Account
   is
      pragma Unreferenced (Password);
   begin
      if Hera.Corporations.Exists (User_Name) then
         return new Authorized_Account'
           (User_Name =>
              Ada.Strings.Unbounded.To_Unbounded_String (User_Name));
      else
         return null;
      end if;
   end Login;

end Hera.Authorization;
