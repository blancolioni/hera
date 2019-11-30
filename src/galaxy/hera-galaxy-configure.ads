with WL.Random.Names;

package Hera.Galaxy.Configure is

   procedure Generate_Galaxy
     (Number_Of_Systems  : Positive;
      Radius_X           : Non_Negative_Real;
      Radius_Y           : Non_Negative_Real;
      Radius_Z           : Non_Negative_Real;
      Names              : WL.Random.Names.Name_Generator);

end Hera.Galaxy.Configure;
