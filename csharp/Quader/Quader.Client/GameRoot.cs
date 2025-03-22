using Microsoft.Extensions.Logging;
using Nez;
using Nez.ImGuiTools;
using Nez.UI;
using Quader.Client.GameConfig;
using Quader.Client.Managers;
using Quader.Client.Scenes;
using Quader.Client.Skins;

namespace Quader.Client;

public class GameRoot : Core
{
    public static bool PausedByDeactivation = false;

    public const string ConfigFilePath = "config.json";

    protected override void Initialize()
    {
        base.Initialize();

        Window.AllowUserResizing = true;

        var pauseMgr = new PauseManager();
        RegisterGlobalManager(pauseMgr);

#if DEBUG
        var imGuiManager = new ImGuiManager();
        imGuiManager.ShowCoreWindow = true;
        imGuiManager.ShowDemoWindow = false;
        imGuiManager.ShowMenuBar = true;
        imGuiManager.ShowSceneGraphWindow = true;
        imGuiManager.ShowSeperateGameWindow = false;
        imGuiManager.ShowStyleEditor = false;
        RegisterGlobalManager(imGuiManager);
        imGuiManager.Enabled = true;


        var dbgMgr = new DebugManager(pauseMgr);
        RegisterGlobalManager(dbgMgr);
#endif

        Activated += (_, _) =>
        {
            /*if (PausedByDeactivation)
            {
                pauseMgr.IsPaused = false;
                PausedByDeactivation = false;
            }*/
        };

        Deactivated += (_, _) =>
        {
            /*if (!pauseMgr.IsPaused)
            {
                pauseMgr.IsPaused = true;
                PausedByDeactivation = true;
            }*/
        };

        PauseOnFocusLost = false;
        DebugRenderEnabled = false;

        var skinTexture = Content.LoadTexture(Nez.Content.Skins.Default_3);
        var boardTexture = Content.LoadTexture(Nez.Content.Skins.Board_default);

        var mainFont = Content.LoadBitmapFont(Nez.Content.Fonts.Main_font);
        var debugFont = Content.LoadBitmapFont(Nez.Content.Fonts.Debug_font);
        var silkscreenFont = Content.LoadBitmapFont(Nez.Content.Fonts.Silkscreen8);

        var skin = Skin.CreateDefaultSkin();
        skin.Add("board", new BoardSkin(
            skinTexture,
            boardTexture,
            mainFont,
            debugFont,
            silkscreenFont
        ));
        Services.AddService(skin);

        Config gameConfig;

        try
        {
            gameConfig = Config.LoadFromFile(ConfigFilePath);
        }
        catch (FileNotFoundException e)
        {
            gameConfig = new Config();
        }

        Services.AddService(gameConfig);

        try
        {
            Scene = new GameplayScene();
        }
        catch (Exception e)
        {
            throw;
        }
    }

    protected override void UnloadContent()
    {
        try
        {
            var config = Services.GetService<Config>();
            Config.SaveToFile(config, ConfigFilePath);
        }
        catch (Exception e)
        {
            Console.WriteLine(e);
        }

        base.UnloadContent();
    }
}