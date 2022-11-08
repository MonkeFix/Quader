using Microsoft.Xna.Framework;
using Nez;
using Nez.UI;
using Quader.Components.Boards;
using Quader.Engine;

namespace Quader.Components.UI;

public class PracticeModeUiComponent : Component, IBoardComponent
{
    private Skin _skin = null!;
    
    public Board Board { get; }

    public PracticeModeUiComponent(Board board)
    {
        Board = board;
    }

    public override void OnAddedToEntity()
    {
        _skin = Core.Services.GetService<Skin>();
        InitUi();
    }

    private void InitUi()
    {
        var uiCanvas = Entity.Scene.CreateEntity("practice-ui-canvas").AddComponent(new UICanvas());
        uiCanvas.IsFullScreen = false;
        
        var table = new Table();

        //table.SetY(100);
        table.Left().Top();
        table.SetFillParent(false);//.SetHeight(64);
        table.Defaults().SetPadTop(4).SetPadBottom(4).SetPadLeft(4).SetPadRight(4).SetAlign(Align.Left).SetMinWidth(80).SetMinHeight(32);
        table.SetBackground(new PrimitiveDrawable(new Color(40, 40, 40, 220)));
        
        var scrollPane = uiCanvas.Stage.AddElement(new ScrollPane(table, _skin));
        scrollPane.Validate();
        scrollPane.SetSize(340 + scrollPane.GetScrollBarWidth(), Screen.Height);

        table.Row().SetPadTop(4); // Leave room for the directions

        table.Add(new TextButton("Hello", _skin));
        table.Row();
        table.Add(new Label("Gravity Increase"));

        var label = new Label("");
        var slider = new Slider(0, 10, .01f, false, _skin);
        slider.Value = Board.GravitySettings.GravityIncrease;
        label.SetText(slider.Value.ToString());
        slider.OnChanged += val =>
        {
            Board.GravitySettings.GravityIncrease = val;
            label.SetText(val.ToString());
        };
        table.Add(slider);
        table.Add(label);

        table.Row();
        table.Add(new CheckBox("Hello", _skin));
        table.Row();
        table.Add(new Label("Hello", _skin));
        //uiCanvas.Stage.AddElement(table);
    }

    
}