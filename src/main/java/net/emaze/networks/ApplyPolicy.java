package net.emaze.networks;

import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.SequencingPolicy;

public class ApplyPolicy<T> implements Delegate<Maybe<T>, Maybe<T>> {

    private final SequencingPolicy<T> policy;

    public ApplyPolicy(SequencingPolicy<T> policy) {
        this.policy = policy;
    }

    @Override
    public Maybe<T> perform(Maybe<T> element) {
        return element.hasValue() ? policy.next(element.value()) : Maybe.<T>nothing();
    }
}
