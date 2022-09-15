using System;
using Microsoft.Xna.Framework;
using Nez;
using Nez.UI;
using static Nez.Content;

namespace Quader.Components.UI;

public class SharedUiComponent : Component
{
    private Skin _skin = null!;
    private SharedActions _sharedActions;

    public SharedUiComponent(SharedActions sharedActions)
    {
        _sharedActions = sharedActions;
    }

    public override void OnAddedToEntity()
    {
        _skin = Core.Services.GetService<Skin>();
        InitUi();
    }

    private void InitUi()
    {
        var uiCanvas = Entity.Scene.CreateEntity("shared-ui-canvas").AddComponent(new UICanvas());
        uiCanvas.IsFullScreen = false;
        UpdateSkin();

        var topMenuTable = new Table();

        //topMenuTable.DebugAll();
        //topMenuTable.SetDebug(true);

        topMenuTable.Left().Top();
        topMenuTable.SetFillParent(false);//.SetHeight(64);
        topMenuTable.Defaults().SetPadTop(4).SetPadBottom(4).SetPadLeft(4).SetPadRight(4).SetAlign(Align.Left).SetMinWidth(80).SetMinHeight(32);
        topMenuTable.SetBackground(new PrimitiveDrawable(new Color(40, 40, 40, 220)));
        
        topMenuTable.Add(new TextButton("New Game", _skin)).SetFillX()
            .GetElement<Button>().OnClicked += b =>
        {
            _sharedActions?.NewGameAction();
        };

        topMenuTable.Add(new TextButton("Replays", _skin)).SetFillX()
            .GetElement<Button>().OnClicked += b =>
        {
            _sharedActions?.OpenReplaysAction();
        };

        topMenuTable.Add(new TextButton("Settings", _skin)).SetFillX()
            .GetElement<Button>().OnClicked += b =>
        {

        };

        topMenuTable.Add(new TextButton("Restart", _skin)).SetFillX()
            .GetElement<Button>().OnClicked += b =>
        {
            _sharedActions?.RestartAction();
        };

        topMenuTable.Add(new TextButton("Quit", _skin)).SetFillX()
            .GetElement<Button>().OnClicked += b =>
        {
            _sharedActions?.QuitAction();
        };

        uiCanvas.Stage.AddElement(topMenuTable);
    }

    private void UpdateSkin()
    {
        var tfs = _skin.Get<TextFieldStyle>();
        tfs.Background.LeftWidth = tfs.Background.RightWidth = 4;
        tfs.Background.BottomHeight = 0;
        tfs.Background.TopHeight = 3;

        var checkbox = _skin.Get<CheckBoxStyle>();
        checkbox.CheckboxOn.MinWidth = checkbox.CheckboxOn.MinHeight = 15;
        checkbox.CheckboxOff.MinWidth = checkbox.CheckboxOff.MinHeight = 15;
        checkbox.CheckboxOver.MinWidth = checkbox.CheckboxOver.MinHeight = 15;
    }
}