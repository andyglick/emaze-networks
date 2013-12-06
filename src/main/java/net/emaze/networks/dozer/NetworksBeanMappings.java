
package net.emaze.networks.dozer;

import net.emaze.networks.Ip;
import net.emaze.networks.Ipv6;
import net.emaze.networks.Ipv6Mask;
import net.emaze.networks.Ipv6Network;
import net.emaze.networks.Mask;
import net.emaze.networks.Network;
import org.dozer.loader.api.BeanMappingBuilder;
import org.dozer.loader.api.TypeMappingOptions;


public class NetworksBeanMappings extends BeanMappingBuilder {

    @Override
    protected void configure() {
        mapping(Ip.class, Ip.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
        mapping(Network.class, Network.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
        mapping(Mask.class, Mask.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
        mapping(Ipv6.class, Ipv6.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
        mapping(Ipv6Network.class, Ipv6Network.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
        mapping(Ipv6Mask.class, Ipv6Mask.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
    }

}
