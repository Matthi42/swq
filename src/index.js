import { main } from "../output/Main";

if (module.hot) {
  module.hot.accept(function () {
    main();
  });
}

main();
