with Hera.Money;

package body Hera.Pops.Classes.Configure is

   Pop_Class_Version : constant Hera.Objects.Object_Version := "0.0.1";

   ----------------------
   -- Create_Pop_Class --
   ----------------------

   procedure Create_Pop_Class (Config : Tropos.Configuration) is

      Consumer_Quality : constant Quality_Type :=
        Quality_Type (Positive'(Config.Get ("consumer-quality")));
      Service_Quality : constant Quality_Type :=
        Quality_Type (Positive'(Config.Get ("service-quality")));

      function Required_Goods return Commodity_Lists.List;

      --------------------
      -- Required_Goods --
      --------------------

      function Required_Goods return Commodity_Lists.List is
      begin
         return List : Commodity_Lists.List do
            for Item of Hera.Commodities.Consumer_Goods loop
               if Item.Quality = Consumer_Quality then
                  List.Append (Item);
               end if;
            end loop;
            for Item of Hera.Commodities.Services loop
               if Item.Quality = Service_Quality then
                  List.Append (Item);
               end if;
            end loop;
         end return;
      end Required_Goods;

      Pop_Class : constant Root_Pop_Class_Object :=
                    Root_Pop_Class_Object'
                      (Hera.Objects.Root_Localised_Object with
                       Consumer_Quality => Consumer_Quality,
                       Service_Quality  => Service_Quality,
                       Required_Goods   => Required_Goods,
                       Salary           =>
                         Hera.Money.To_Price (Config.Get ("salary")));

   begin

      New_Pop_Class
        (Pop_Class_Type
           (Pop_Class.New_Localised_Object
                (Version => Pop_Class_Version,
                 Tag     => Config.Config_Name)));
   end Create_Pop_Class;

end Hera.Pops.Classes.Configure;
