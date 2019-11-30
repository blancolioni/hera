package Hera.UI.Models.Loader is

   function Exists (Model_Name : String) return Boolean;

   function Get
     (Model_Name : String)
      return Hera_Model
     with Pre => Exists (Model_Name);

end Hera.UI.Models.Loader;
