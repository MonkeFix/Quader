using LiteLog.Logging;
using Nez;
using Nez.Persistence;
using Nez.UI;
using Quader.Components.Startup;
using Quader.Config;
using Quader.Engine.Pieces;
using Quader.Skinning;

namespace Quader.Scenes;

/// <summary>
/// The scene where the game starts at. Initializes all the necessary variables, loads content, prerendered textures, etc. 
/// </summary>
public class StartupScene : SceneBase
{
    private readonly ILogger _logger = LoggerFactory.GetLogger<StartupScene>();
    
    public StartupScene() : base(true, true)
    {
        
    }

    public override void Initialize()
    {
        base.Initialize();
        
        _logger.Info("Loading Assets...");

        // TODO: Add proper UI
        CreateEntity("loading")
            .AddComponent(new LoadingComponent());
        
        LoadContentAsync((scene) =>
        {
            _logger.Info("Successfully loaded assets, going to Gameplay Scene");
            Core.StartSceneTransition(new FadeTransition(() => new GameplayScene()));
        }, this);
    }

    private void LoadContentAsync(Action<StartupScene> onLoaded, StartupScene context)
    {
        var syncContext = SynchronizationContext.Current;

        Task.Run(() =>
        {
            var pieceSettingsFilename = Path.Combine("Content", "data", "default_piece_settings.json");
            PieceSettings pieceSettings;

            if (!File.Exists(pieceSettingsFilename))
            {
                _logger.Warn($"Piece Settings file does not exist ({pieceSettingsFilename}), taking defaults");
                using var sw = new StreamWriter(pieceSettingsFilename, false);
                pieceSettings = new PieceSettings();
                sw.WriteLine(Json.ToJson(pieceSettings, GameRoot.DefaultJsonSettings));
            }
            else
            {
                using var sr = new StreamReader(pieceSettingsFilename);
                var json = sr.ReadToEnd();
                pieceSettings = Json.FromJson<PieceSettings>(json, GameRoot.DefaultJsonSettings);
                _logger.Info("Successfully loaded Piece Settings from file");
            }

            PieceUtils.PieceSettings = pieceSettings;
        
            _logger.Info("Creating skin");
            Skin skin = Skin.CreateDefaultSkin();

            _logger.Info("Loading content files");

            var skinTexture = Content.LoadTexture(Nez.Content.Skins.Default_3);
            var boardTexture = Content.LoadTexture(Nez.Content.Skins.Board_default);
            var mainFont = Content.LoadBitmapFont(Nez.Content.Fonts.Main_font, true);
            var debugFont = Content.LoadBitmapFont(Nez.Content.Fonts.Debug_font, true);
            var silkscreenFont = Content.LoadBitmapFont(Nez.Content.Fonts.Silkscreen8, true);
            skin.Add("default", new BoardSkin(skinTexture, boardTexture, mainFont, debugFont, silkscreenFont));
            Core.Services.AddService(skin);
        
            _logger.Info($"Loading Game Config ({GameRoot.ConfigFilePath})");
            GameConfig gc;

            try
            {
                gc = GameConfig.LoadFromFile(GameRoot.ConfigFilePath);
            }
            catch (FileNotFoundException e)
            {
                _logger.Info("Config file was not found, taking the defaults");
                gc = new GameConfig();
            }

            Core.Services.AddService(gc);
            
            syncContext?.Post(state => { onLoaded(context); }, null);
        });
        
        
    }
}