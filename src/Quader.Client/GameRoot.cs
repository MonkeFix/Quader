using LiteLog.Logging;
using Microsoft.Xna.Framework;
using Nez;
using Nez.ImGuiTools;
using Nez.Persistence;
using Nez.Persistence.Binary;
using Nez.UI;
using Quader.Config;
using Quader.Engine.Pieces;
using Quader.Scenes;
using Quader.Serialization;
using Quader.Skinning;

namespace Quader;

public class GameRoot : Core
{
    private readonly string _configFilePath = "config.json";

    public static JsonSettings DefaultJsonSettings { get; private set; } = null!;

    private readonly ILogger _logger = LoggerFactory.GetLogger<GameRoot>();

    public static readonly int TargetFps = 240;

    private FileDataStore _dataStore;

    public GameRoot() : base(windowTitle: "Quader")
    {
        _logger.Debug("Setting up");

        Window.AllowUserResizing = false;

        IsFixedTimeStep = true;
        TargetElapsedTime = TimeSpan.FromSeconds(1.0 / TargetFps);

        //_logger.Info("Initializing FMOD");
        //FMODManager.Init(FMODMode.Core, "Content");

        _logger.Info("Loading Piece Settings");
        DefaultJsonSettings = new JsonSettings
        {
            TypeConverters = new JsonTypeConverter[]
            {
                new ColorJsonConverter(),
                new ColorFactoryConverter()
            },
            PrettyPrint = true
        };

        var pieceSettingsFilename = Path.Combine("Content", "data", "default_piece_settings.json");
        PieceSettings pieceSettings;

        if (!File.Exists(pieceSettingsFilename))
        {
            _logger.Warn($"Piece Settings file does not exist ({pieceSettingsFilename}), taking the defaults");
            using var sw = new StreamWriter(pieceSettingsFilename, false);
            pieceSettings = new PieceSettings();
            sw.WriteLine(Json.ToJson(pieceSettings, DefaultJsonSettings));
        }
        else
        {
            using var sr = new StreamReader(pieceSettingsFilename);
            var json = sr.ReadToEnd();
            pieceSettings = Json.FromJson<PieceSettings>(json, DefaultJsonSettings);
            _logger.Info("Successfully loaded Piece Settings from file");
        }

        PieceUtils.PieceSettings = pieceSettings;

        PauseOnFocusLost = false;
        
        // Creating necessary directories
        Directory.CreateDirectory(Path.Combine(Storage.GetStorageRoot(), "Replays"));
    }
    
    protected override void Initialize()
    {
        _logger.Debug("Started initializing...");

        base.Initialize();

        _dataStore = new FileDataStore(Storage.GetStorageRoot());
        Services.AddService(_dataStore);

        Window.AllowUserResizing = true;

        var imGuiManager = new ImGuiManager();
        imGuiManager.ShowCoreWindow = true;
        imGuiManager.ShowDemoWindow = false;
        imGuiManager.ShowMenuBar = false;
        imGuiManager.ShowSceneGraphWindow = true;
        imGuiManager.ShowSeperateGameWindow = false;
        imGuiManager.ShowStyleEditor = false;
        imGuiManager.Enabled = true;
        RegisterGlobalManager(imGuiManager);

#if DEBUG
        //RegisterGlobalManager(new DebugManager());
#endif

        PauseOnFocusLost = false;

        _logger.Info("Creating skin");
        Skin skin = Skin.CreateDefaultSkin();

        _logger.Info("Loading content files");

        var skinTexture = Content.LoadTexture(Nez.Content.Skins.Default_3);
        var boardTexture = Content.LoadTexture(Nez.Content.Skins.Board_default);
        var mainFont = Content.LoadBitmapFont(Nez.Content.Fonts.Main_font, true);
        var debugFont = Content.LoadBitmapFont(Nez.Content.Fonts.Debug_font, true);
        skin.Add("default", new BoardSkin(skinTexture, boardTexture, mainFont, debugFont));
        Services.AddService(typeof(Skin), skin);
        
        _logger.Info($"Loading Game Config ({_configFilePath})");
        GameConfig gc;

        try
        {
            gc = GameConfig.LoadFromFile(_configFilePath);
        }
        catch (FileNotFoundException e)
        {
            _logger.Info("Config file was not found, taking the defaults");
            gc = new GameConfig();
        }

        Services.AddService(gc);

        _logger.Debug("Setting up scene");
        try
        {
            Scene = new GameplayScene();
        }
        catch (Exception e)
        {
            _logger.Log("FATAL: " + e.Message, LogLevel.Error);
            throw;
        }

        _logger.Debug("End initializing");
    }

    protected override void UnloadContent()
    {
        _logger.Info("Unloading content");

        var gc = Services.GetService<GameConfig>();
        GameConfig.SaveToFile(gc, _configFilePath);

        //KeyValueDataStore.Default.Flush(_dataStore);

        // FMODManager.Unload();
    }

    protected override void Update(GameTime gameTime)
    {
        base.Update(gameTime);
        
        // FMODManager.Update();
    }
}