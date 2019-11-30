with Hera.Identifiers;

with Hera.Handles.Account;

package body Hera.Accounts.Configure is

   Account_Version : constant := 1;

   -----------------
   -- New_Account --
   -----------------

   function New_Account
     (Start_Cash : Hera.Money.Money_Type)
      return Account_Handle
   is
   begin
      return Hera.Handles.Account.Create
        (Version    => Account_Version,
         Identifier => Hera.Identifiers.Next_Identifier,
         Cash       => Start_Cash);
   end New_Account;

end Hera.Accounts.Configure;
