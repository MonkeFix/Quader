using System;
using System.Reflection;
using Microsoft.Xna.Framework;
using Nez;
using Nez.UI;
using Quader.Components.Boards;
using Quader.Engine;
using Quader.Engine.Settings;

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
        table.Defaults().SetPadTop(2).SetPadBottom(4).SetPadLeft(8).SetPadRight(4).SetAlign(Align.Left).SetMinWidth(80).SetMinHeight(32);
        table.SetBackground(new PrimitiveDrawable(new Color(40, 40, 40, 220)));
        
        var scrollPane = uiCanvas.Stage.AddElement(new ScrollPane(table, _skin));
        scrollPane.Validate();
        scrollPane.SetSize(340 + scrollPane.GetScrollBarWidth(), Screen.Height);

        table.Row().SetPadTop(2); // Leave room for the directions

        table.Add(new TextButton("1", _skin));
        table.Row();
        MakeSection(table, _skin, "Gravity Settings");
        table.Row();
        MakeSlider(table, _skin, "Gravity Increase", 0, 10, 0.001f, SettingsType.Gravity, "GravityIncrease");
        table.Row();
        MakeSlider(table, _skin, "Base Gravity", 0, 10, 0.01f, SettingsType.Gravity, "BaseGravity");
        table.Row();
        MakeSlider(table, _skin, "Lock Delay", 0, 10, 0.1f, SettingsType.Gravity, "LockDelay");
        table.Row();
        MakeSlider(table, _skin, "Constant Gravity", 0, 10, 0.1f, SettingsType.Gravity, "ConstantGravity");
        table.Row();
        
        MakeSection(table, _skin, "Attack Settings");
        table.Row();
        MakeSlider(table, _skin, "0 Lines", 0, 24, 1, SettingsType.Attack, "Lines0");
        table.Row();
        MakeSlider(table, _skin, "1 Line", 0, 24, 1, SettingsType.Attack, "Lines1");
        table.Row();
        MakeSlider(table, _skin, "2 Lines", 0, 24, 1, SettingsType.Attack, "Lines2");
        table.Row();
        MakeSlider(table, _skin, "3 Lines", 0, 24, 1, SettingsType.Attack, "Lines3");
        table.Row();
        MakeSlider(table, _skin, "4 Lines", 0, 24, 1, SettingsType.Attack, "Lines4");
        table.Row();
        MakeSlider(table, _skin, "T-Spin Single", 0, 24, 1, SettingsType.Attack, "TSpinSingle");
        table.Row();
        MakeSlider(table, _skin, "T-Spin Double", 0, 24, 1, SettingsType.Attack, "TSpinDouble");
        table.Row();
        MakeSlider(table, _skin, "T-Spin Triple", 0, 24, 1, SettingsType.Attack, "TSpinTriple");
        table.Row();
        MakeSlider(table, _skin, "T-Spin Single Mini", 0, 24, 1, SettingsType.Attack, "TSpinSingleMini");
        table.Row();
        MakeSlider(table, _skin, "All Clear", 0, 24, 1, SettingsType.Attack, "AllClear");
        table.Row();
        MakeSlider(table, _skin, "Garbage Delay Ms", 0, 100_000, 100, SettingsType.Attack, "GarbageDelayMs");

        //uiCanvas.Stage.AddElement(table);
    }

    enum SettingsType
    {
        Gravity, 
        Attack,
        Board
    }
    
    private void MakeSlider(Table table, Skin skin, string label, float min, float max, float step, SettingsType type, string propName)
    {
        var t = type switch
        {
            SettingsType.Gravity => typeof(GravitySettings),
            SettingsType.Attack => typeof(AttackSettings),
            SettingsType.Board => typeof(BoardSettings),
            _ => throw new ArgumentOutOfRangeException(nameof(type), type, null)
        };

        MakeSlider(table, skin, label, min, max, step, t, GetRequiredObj(type), propName);
    }
    
    private void MakeSlider(Table table, Skin skin, string label, float min, float max, float step, Type type, object obj, string propName)
    {
        FieldInfo fieldInfo = type.GetField(propName);
        
        //float value = Convert.ToSingle(fieldInfo.GetValue(_particleEmitterConfig));
        float value = Convert.ToSingle(fieldInfo!.GetValue(
            obj
        ));
        
        var slider = new Slider(skin, null, min, max);
        var textBox = new TextField(value.ToString(), skin);
        
        slider.SetStepSize(step);
        slider.SetValue(value);
        slider.OnChanged += newValue =>
        {
            fieldInfo.SetValue(obj, TypedConvert(fieldInfo, newValue));
            textBox.SetText(newValue.ToString());
        };

        textBox.SetMaxLength(5);
        textBox.OnTextChanged += (field, str) =>
        {
            if (float.TryParse(str, out float newValue))
            {
                fieldInfo.SetValue(obj, TypedConvert(fieldInfo, newValue));
                slider.SetValue(newValue);
            }
        };

        table.Add(label).Left().Width(140);
        table.Add(textBox).Left().Width(50);
        table.Add(slider).Left();
    }
    
    void MakeSection(Table table, Skin skin, string sectionName)
    {
        var label = new Label(sectionName, skin);
        label.SetFontColor(new Color(241, 156, 0));
        table.Add(label).SetPadTop(20);
    }

    private object GetRequiredObj(SettingsType type)
    {
        return type switch
        {
            SettingsType.Gravity => Board.GameSettings.Gravity,
            SettingsType.Attack => Board.GameSettings.Attack,
            SettingsType.Board => Board.GameSettings.Board,
            _ => throw new ArgumentOutOfRangeException(nameof(type), type, null)
        };
    }
    
    private object? TypedConvert(FieldInfo fieldInfo, object value)
    {
        if (fieldInfo.FieldType == typeof(float))
            return (float)value;

        if (fieldInfo.FieldType == typeof(int))
            return (int)Convert.ToSingle(value);

        if (fieldInfo.FieldType == typeof(uint))
            return (uint)Convert.ToSingle(value);

        return Convert.ToString(value);
    }
}