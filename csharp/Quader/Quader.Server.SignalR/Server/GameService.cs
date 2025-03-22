using System.Threading.Channels;
using Microsoft.AspNetCore.SignalR;

namespace Quader.Server.SignalR.Server;

public class GameService
{
    private readonly Channel<string> _messageChannel = Channel.CreateUnbounded<string>();

    public Channel<string> Listen()
    {
        return _messageChannel;
    }

    public async Task BroadcastMessageAsync(string message)
    {
        await _messageChannel.Writer.WriteAsync(message);
    }

    public async Task ProcessMessageAsync(IHubContext<GameHub> hubContext,
        CancellationToken cancellationToken)
    {
        await foreach (var message in _messageChannel.Reader.ReadAllAsync(cancellationToken))
        {
            await hubContext.Clients.All.SendAsync("ReceiveMessage", message, cancellationToken);
        }
    }
}