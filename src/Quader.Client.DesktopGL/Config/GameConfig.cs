using System.IO;
using Nez.Persistence;

namespace Quader.Config
{
    public class GameConfig
    {
        [JsonInclude] public Handling Handling { get; set; } = new Handling().Defaults();
        [JsonInclude] public Controls Controls { get; set; } = new Controls().Defaults();

        
        public static GameConfig LoadFromFile(string filename)
        {
            using var sr = new StreamReader(filename);

            var text = sr.ReadToEnd();

            return Json.FromJson<GameConfig>(text);
        }

        public static void SaveToFile(GameConfig config, string filename)
        {
            using var sw = new StreamWriter(filename);

            var text = Json.ToJson(config, true);

            sw.WriteLine(text);
        }
    }
}