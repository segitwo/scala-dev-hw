package module4.homework.services

import zio.Has
import zio.Task
import module4.homework.dao.entity.{Role, RoleCode, User, UserId, UserToRole}
import module4.homework.dao.repository.UserRepository
import zio.ZIO
import zio.RIO
import zio.ZLayer
import zio.macros.accessible
import module4.phoneBook.db
import module4.phoneBook.db.DataSource

import java.sql.SQLException
import javax.sql

@accessible
object UserService{
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service{
        val dc = db.Ctx
        import dc._

        def listUsers(): RIO[db.DataSource, List[User]] =
        userRepo.list()


        def listUsersDTO(): RIO[db.DataSource,List[UserDTO]] = {
          val lud  =
            for {
              lu <- listUsers()
              lud = lu.map(u => for {
                ur <- userRepo.userRoles(UserId(u.id)).map(_.toSet)
              } yield UserDTO(u, ur))
            } yield lud
          lud.flatMap(lud => ZIO.collectAll(lud))
        }

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] =
            dc.transaction(
                for {
                    user <- userRepo.createUser(user)
                    _ <- userRepo.insertRoleToUser(roleCode, UserId(user.id))
                    userRoles <- userRepo.userRoles(UserId(user.id)).map(_.toSet)
                } yield UserDTO(user, userRoles)
            )

        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] = {
          val lud =
            for {
              lu <- userRepo.listUsersWithRole(roleCode)
              lud = lu.map(u => for {
                ur <- userRepo.userRoles(UserId(u.id)).map(_.toSet)
              } yield UserDTO(u, ur))
            } yield lud
          lud.flatMap(lud => ZIO.collectAll(lud))
        }


}

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] =
        ZLayer.fromService[UserRepository.Service, UserService.Service](repo => new Impl(repo))
}

case class UserDTO(user: User, roles: Set[Role])