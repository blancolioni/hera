private package Hera.Sessions.Status is

   function Get_Update_Speed
     (Session : Root_Hera_Session'Class)
      return Json.Json_Value'Class;

   procedure Set_Update_Speed
     (Session : in out Root_Hera_Session'Class;
      Value   : Json.Json_Value'Class);

end Hera.Sessions.Status;
