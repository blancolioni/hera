generic
   type Reference_Type is private;
   type Element_Type is private;

   with function Get_Name (Element : Element_Type) return String;
   with function Get_
package Hera.File_System.Tables is

   function Table_Node return Node_Interface'Class;

end Hera.File_System.Tables;
