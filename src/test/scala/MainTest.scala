import java.nio.file.{Files, Path, Paths}
import java.util.Comparator

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MainTest extends AnyFlatSpec with Matchers{
  implicit val fs: RealFS[Id] = new RealFS[Id]
  implicit val pp: PrettyPrinter[Id] = new PrettyPrinter[Id]
  val execution = new Execution[Id, Path, Path]

  val tmp_path: Path = Paths.get("./tmp")
  Files.createDirectories(tmp_path)

  execution.run(tmp_path)

  val testDir: Path = tmp_path.resolve("test_dir")
  val dirB: Path = testDir.resolve("b")
  val dirF: Path = testDir.resolve("f")
  val foo: Path = dirF.resolve("foo")
  val bar: Path = dirB.resolve("bar")
  val baz: Path = dirB.resolve("baz")

  Files.exists(testDir) shouldBe true
  Files.exists(dirB) shouldBe true
  Files.exists(dirF) shouldBe true
  Files.exists(foo) shouldBe true
  Files.exists(bar) shouldBe true
  Files.exists(baz) shouldBe true

  Files.walk(tmp_path).sorted(Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

}
