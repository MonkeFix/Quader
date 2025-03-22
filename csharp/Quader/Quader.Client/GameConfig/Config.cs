namespace Quader.Client.GameConfig;

public class Config
{
    public Handling Handling = new Handling();
    public Controls Controls = new Controls();
    public Audio Audio = new Audio();
    public Gameplay Gameplay = new Gameplay();

    public static Config LoadFromFile(string filename)
    {
        var jsonStr = File.ReadAllText(filename);

        return Nez.Persistence.Json.FromJson<Config>(jsonStr);
    }

    public static void SaveToFile(Config config, string filename)
    {
        var jsonStr = Nez.Persistence.Json.ToJson(config, true);
        File.WriteAllText(filename, jsonStr);
    }
}