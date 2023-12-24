using Quader.WebApi.Models.Users;

namespace Quader.WebApi.Services.Auth;

public interface ITokenGeneratorService
{
    string Generate(User user);
}