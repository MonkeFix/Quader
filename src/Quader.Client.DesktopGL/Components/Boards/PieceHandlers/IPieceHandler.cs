using Nez;

namespace Quader.Components.Boards.PieceHandlers
{
    public interface IPieceHandler : IUpdatable, IResetable
    {
        void Start();
    }
}