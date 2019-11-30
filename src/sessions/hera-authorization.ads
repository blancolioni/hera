with Hera.UI;

package Hera.Authorization is

   function Login
     (User_Name : String;
      Password  : String)
      return Hera.UI.UI_Account;

end Hera.Authorization;
