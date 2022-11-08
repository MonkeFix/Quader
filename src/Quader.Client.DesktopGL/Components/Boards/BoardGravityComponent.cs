using Nez;
using Quader.Debugging.Logging;
using Quader.Engine;

namespace Quader.Components.Boards
{
    public class BoardGravityComponent : Component, IUpdatable, IBoardComponent, IBoardToggleable
    {
        public Board Board { get; }

        private TimeManagerComponent _timeManager = null!;
        
        private readonly ILogger _logger = LoggerFactory.GetLogger<BoardGravityComponent>();

        public BoardGravityComponent(Board board)
        {
            Board = board;
        }

        public override void OnAddedToEntity()
        {
            _timeManager = Entity.GetComponent<TimeManagerComponent>();
        }

        public void Enable()
        {
            _logger.Trace("Enabling");
            Enabled = true;
        }

        public void Disable()
        {
            _logger.Trace("Disabling");
            Enabled = false;
        }

        public void Update()
        {
            Board.UpdateGravity(_timeManager.DeltaTime, _timeManager.ElapsedMilliseconds);
        }
    }
}