using System.Collections.Generic;
using Nez.Persistence;

namespace Quader.Engine.Settings;

public class GameSettings
{
    private static GameSettings? _default;

    public static GameSettings Default
    {
        get
        {
            if (_default == null)
                _default = new GameSettings
                {
                    Board = new BoardSettings
                    {
                        BoardWidth = 10,
                        BoardHeight = 20,
                    },
                    Gravity = new GravitySettings
                    {
                        BaseGravity = 0.8f,
                        GravityIncrease = 0.007f,
                        LockDelay = 1f,
                        ConstantGravity = 0
                    },
                    Attack = new AttackSettings
                    {
                        Lines0 = 0,
                        Lines1 = 0,
                        Lines2 = 1,
                        Lines3 = 2,
                        Lines4 = 4,
                        TSpinDouble = 4,
                        TSpinTriple = 6,
                        TSpinSingle = 2,
                        TSpinSingleMini = 1,
                        AllClear = 10,
                        BackToBacks = new List<int> {1,2,3,4,5},
                        Combos = new List<int> {1,2,3,4,5},
                        GarbageDelayMs = 500
                    }
                };

            return _default;
        }
    }
    public GravitySettings Gravity = null!;
    public BoardSettings Board = null!;
    public AttackSettings Attack = null!;
}