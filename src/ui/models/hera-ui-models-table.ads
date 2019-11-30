package Hera.UI.Models.Table is

   type Root_Table_Model is new Root_Hera_Model with private;

   overriding function Handle
     (Model   : in out Root_Table_Model;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class;

private

   type Root_Table_Model is new Root_Hera_Model with
      record
         null;
      end record;

end Hera.UI.Models.Table;
