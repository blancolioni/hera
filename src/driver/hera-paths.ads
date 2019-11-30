package Hera.Paths is

   Config_Path : constant String :=
     "E:\home\fraser\Documents\GitHub\hera\config";

   function Config_File
     (File_Path : String)
     return String
   is (Config_Path & "/" & File_Path);

end Hera.Paths;
