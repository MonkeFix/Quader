using Nez;
using Quader.Engine;
using Quader.Engine.Boards;

namespace Quader.Client.Components.Boards;

public class BoardUpdater : Component, IUpdatable
{
    private Board _board;

    public TimeManager TimeManager { get; private set; } = null!;

    public BoardUpdater(Board board)
    {
        _board = board;
    }

    public override void OnAddedToEntity()
    {
        TimeManager = Core.Scene.GetSceneComponent<TimeManagerComponent>().TimeManager;
    }

    public void Update()
    {
        TimeManager.Update(Time.DeltaTime);
        _board.Update(TimeManager, out _);
    }
}