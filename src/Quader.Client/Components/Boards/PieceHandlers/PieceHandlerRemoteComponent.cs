using Nez;
using Quader.Engine;

namespace Quader.Components.Boards.PieceHandlers;

public class PieceHandlerRemoteComponent : Component, IPieceHandler, IBoardComponent, IBoardToggleable
{
    public Board Board { get; }

    public PieceHandlerRemoteComponent(Board board)
    {
        Board = board;
    }

    public void Update()
    {
        throw new System.NotImplementedException();
    }

    public void Start()
    {
        throw new System.NotImplementedException();
    }

    
    public void Enable()
    {
        Enabled = true;
    }

    public void Disable()
    {
        Enabled = false;
    }
}