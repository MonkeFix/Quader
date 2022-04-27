using Quader.WebApi.Models.Auth;

namespace Quader.WebApi.Services.Auth;

public interface IAuthService
{
    Task<AuthResponse?> AuthAsync(AuthRequest request);
}