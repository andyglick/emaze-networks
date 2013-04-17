
package net.emaze.networks.dozer;

import net.emaze.networks.Ip;
import net.emaze.networks.Mask;
import net.emaze.networks.Network;
import net.emaze.networks.dozer.ImmutableBeanFactory;
import org.dozer.loader.api.BeanMappingBuilder;
import org.dozer.loader.api.TypeMappingOptions;


public class NetworksBeanMappings extends BeanMappingBuilder {

    @Override
    protected void configure() {
        mapping(Ip.class, Ip.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
        mapping(Network.class, Network.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
        mapping(Mask.class, Mask.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
    }

}
