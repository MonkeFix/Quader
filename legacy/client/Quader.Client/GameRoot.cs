using LiteLog.Logging;
using Microsoft.Xna.Framework;
using Nez;
using Nez.ImGuiTools;
using Nez.Persistence;
using Nez.Persistence.Binary;
using Quader.Config;
using Quader.Scenes;
using Quader.Serialization;

namespace Quader;

public class GameRoot : Core
{
    public static readonly string ConfigFilePath = "config.json";
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

        _logger.Debug("Setting up scene");
        try
        {
            Scene = new StartupScene();  // new GameplayScene();
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
        GameConfig.SaveToFile(gc, ConfigFilePath);

        //KeyValueDataStore.Default.Flush(_dataStore);

        // FMODManager.Unload();
    }

    protected override void Update(GameTime gameTime)
    {
        base.Update(gameTime);
        
        // FMODManager.Update();
    }
}