using System.Collections.Generic;
using Nez.Persistence;

namespace Quader.Engine.Settings;

public class AttackSettings
{
    [JsonInclude] public int Lines0 { get; set; }
    [JsonInclude] public int Lines1 { get; set; }
    [JsonInclude] public int Lines2 { get; set; }
    [JsonInclude] public int Lines3 { get; set; }
    [JsonInclude] public int Lines4 { get; set; }
    [JsonInclude] public int TSpinSingle { get; set; }
    [JsonInclude] public int TSpinDouble { get; set; }
    [JsonInclude] public int TSpinTriple { get; set; }
    [JsonInclude] public int TSpinSingleMini { get; set; }
    [JsonInclude] public int AllClear { get; set; }
    [JsonInclude] public List<int> BackToBacks { get; set; }
    [JsonInclude] public List<int> Combos { get; set; }
}