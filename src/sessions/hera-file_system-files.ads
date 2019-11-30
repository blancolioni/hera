package Hera.File_System.Files is

   function File_Node
     (Contents : String := "")
      return Node_Interface'Class;

   type File_Contents_Function is
     access function return String;

   function Dynamic_File_Node
     (Contents : File_Contents_Function)
      return Node_Interface'Class;

end Hera.File_System.Files;
