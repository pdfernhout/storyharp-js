const o = require("ospec")

const KfCommand = require("../dist/KfCommand").KfCommand

o.spec("KfCommand", () => {

	o("new", () => {
		const command = new KfCommand()
		o(command).notEquals(null)
	})

	o("do and redo", () => {
		const command = new KfCommand()
		o(command).notEquals(null)
		o(command.done).equals(false)

		command.doCommand()
		o(command.done).equals(true)

		command.undoCommand()
		o(command.done).equals(false)

		command.redoCommand()
		o(command.done).equals(true)

		command.undoCommand()
		o(command.done).equals(false)
	})

})

/*

public class Test_KfCommand {

	@Test
	public void testKfCommand() {
		KfCommand command = new KfCommand();
		assertTrue(command != null);
		assertTrue(!command.done);
		command.doCommand();
		assertTrue(command.done);
		command.undoCommand();
		assertTrue(!command.done);
		command.redoCommand();
		assertTrue(command.done);
		command.undoCommand();
		assertTrue(!command.done);
	}

}

*/
