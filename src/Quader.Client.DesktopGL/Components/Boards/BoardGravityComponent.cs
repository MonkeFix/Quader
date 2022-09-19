using Nez;
using Quader.Engine;

namespace Quader.Components.Boards
{
    public class BoardGravityComponent : Component, IUpdatable, IBoardComponent, IBoardToggleable
    {
        public Board Board { get; }

        private TimeManagerComponent _timeManager = null!;

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
            Enabled = true;
        }

        public void Disable()
        {
            Enabled = false;
        }

        public void Toggle()
        {
            Enabled = !Enabled;
        }

        public void Update()
        {
            Board.UpdateGravity(_timeManager.DeltaTime, _timeManager.ElapsedMilliseconds);
        }
    }
}