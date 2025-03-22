using System.Threading.Channels;
using Microsoft.AspNetCore.SignalR;

namespace Quader.Server.SignalR.Server;

public class GameHub : Hub
{
    private readonly GameService _gameService;
    private readonly ILogger<GameHub> _logger;

    public GameHub(GameService gameService, ILogger<GameHub> logger)
    {
        _gameService = gameService;
        _logger = logger;
    }

    public ChannelReader<string> Listen()
    {
        return _gameService.Listen().Reader;
    }

    public async Task SendMessage(string message)
    {
        _logger.LogInformation($"{Context.ConnectionId} sent a message: '{message}'");
        await _gameService.BroadcastMessageAsync(message);
    }

    public override async Task OnConnectedAsync()
    {
        _logger.LogInformation($"{Context.ConnectionId} connected");
        await Clients.Caller.SendAsync("Connected", "Welcome to the game server!");
        await base.OnConnectedAsync();
    }

    public override async Task OnDisconnectedAsync(Exception? exception)
    {
        _logger.LogInformation($"{Context.ConnectionId} disconnected");
        await Clients.All.SendAsync("Disconnected", $"{Context.ConnectionId} has lest the server.");
        await base.OnDisconnectedAsync(exception);
    }
}