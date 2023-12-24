using LiteLog.Logging;
using Microsoft.Xna.Framework;
using Nez;
using Quader.Components.UI;
using Quader.Engine.Replays;

namespace Quader.Scenes;

public class ReplayScene : SceneBase
{
    public BoardMoveHolder? Replay { get; set; }

    private readonly ILogger _logger = LoggerFactory.GetLogger<ReplayScene>();

    private SharedActions _sharedActions;

    public ReplayScene(SharedActions sharedActions) : base(true, true)
    {
        _sharedActions = sharedActions;
    }

    public override void Initialize()
    {
        base.Initialize();

        _logger.Debug("Initializing");

        CreateEntity("shared-ui").AddComponent(new SharedUiComponent(_sharedActions));
    }
}